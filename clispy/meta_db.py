import os
import json
import sqlite3
import threading
from contextlib import contextmanager
from pathlib import Path


DEFAULT_DB_PATH = os.environ.get(
    "CLISPY_META_DB", os.path.expanduser("~/.clispy/meta.db")
)


SCHEMA_SQL = """
-- entities: what the doc attaches to (function, variable, class, etc.)
CREATE TABLE IF NOT EXISTS entities (
  id            INTEGER PRIMARY KEY,
  kind          TEXT NOT NULL,
  package       TEXT,
  name          TEXT NOT NULL,
  extra         TEXT,
  UNIQUE(kind, package, name)
);

-- docs: docstring per entity + doc-type
CREATE TABLE IF NOT EXISTS docs (
  id            INTEGER PRIMARY KEY,
  entity_id     INTEGER NOT NULL REFERENCES entities(id) ON DELETE CASCADE,
  doc_type      TEXT NOT NULL,
  lang          TEXT,
  content       TEXT,
  created_at    TEXT NOT NULL DEFAULT (datetime('now')),
  updated_at    TEXT NOT NULL DEFAULT (datetime('now')),
  UNIQUE(entity_id, doc_type)
);

-- history: record of changes and other runtime events
CREATE TABLE IF NOT EXISTS history (
  id            INTEGER PRIMARY KEY,
  entity_id     INTEGER,
  event_type    TEXT NOT NULL,
  payload       TEXT,
  created_at    TEXT NOT NULL DEFAULT (datetime('now'))
);

-- kv: misc metadata (migration version, etc.)
CREATE TABLE IF NOT EXISTS kv (
  k TEXT PRIMARY KEY,
  v TEXT
);

CREATE INDEX IF NOT EXISTS idx_docs_entity ON docs(entity_id);
CREATE INDEX IF NOT EXISTS idx_history_entity ON history(entity_id);
"""


class MetaDB:
    _instance = None
    _lock = threading.Lock()

    def __init__(self, db_path=DEFAULT_DB_PATH):
        self.db_path = db_path
        Path(os.path.dirname(db_path)).mkdir(parents=True, exist_ok=True)
        # relax same-thread check; ensure exclusivity via _lock
        self.conn = sqlite3.connect(self.db_path, check_same_thread=False)
        self.conn.execute("PRAGMA journal_mode=WAL;")
        self.conn.execute("PRAGMA foreign_keys=ON;")
        self._init_schema()

    @classmethod
    def get(cls):
        with cls._lock:
            if cls._instance is None:
                cls._instance = MetaDB()
            return cls._instance

    def _init_schema(self):
        cur = self.conn.cursor()
        cur.executescript(SCHEMA_SQL)
        self.conn.commit()

    @contextmanager
    def tx(self):
        with self._lock:
            try:
                yield self.conn
                self.conn.commit()
            except Exception:
                self.conn.rollback()
                raise

    # --- Entity helpers ---
    def _upsert_entity(
        self, kind: str, package: str | None, name: str, extra: dict | None = None
    ) -> int:
        extra_s = json.dumps(extra) if extra else None
        with self.tx() as cx:
            cx.execute(
                """INSERT INTO entities(kind, package, name, extra)
                     VALUES(?,?,?,?)
                     ON CONFLICT(kind,package,name) DO UPDATE SET extra=COALESCE(excluded.extra, entities.extra)
                """,
                (kind, package, name, extra_s),
            )
            row = cx.execute(
                "SELECT id FROM entities WHERE kind=? AND package IS ? AND name=?",
                (kind, package, name),
            ).fetchone()
            return row[0]

    def _get_entity_id(self, kind: str, package: str | None, name: str) -> int | None:
        row = self.conn.execute(
            "SELECT id FROM entities WHERE kind=? AND package IS ? AND name=?",
            (kind, package, name),
        ).fetchone()
        return row[0] if row else None

    # --- Docs CRUD ---
    def set_doc(
        self, kind, package, name, doc_type, content: str | None, lang: str | None = None
    ):
        eid = self._upsert_entity(kind, package, name)
        with self.tx() as cx:
            cx.execute(
                """INSERT INTO docs(entity_id, doc_type, lang, content)
                       VALUES(?,?,?,?)
                       ON CONFLICT(entity_id, doc_type)
                       DO UPDATE SET content=excluded.content, updated_at=datetime('now')""",
                (eid, doc_type, lang, content),
            )
            cx.execute(
                "INSERT INTO history(entity_id, event_type, payload) VALUES(?,?,?)",
                (
                    eid,
                    "doc:set" if content is not None else "doc:unset",
                    json.dumps({"doc_type": doc_type, "lang": lang}),
                ),
            )

    def get_doc(self, kind, package, name, doc_type) -> str | None:
        eid = self._get_entity_id(kind, package, name)
        if eid is None:
            return None
        row = self.conn.execute(
            "SELECT content FROM docs WHERE entity_id=? AND doc_type=?",
            (eid, doc_type),
        ).fetchone()
        return row[0] if row and row[0] is not None else None

    def delete_doc(self, kind, package, name, doc_type):
        eid = self._get_entity_id(kind, package, name)
        if eid is None:
            return
        with self.tx() as cx:
            cx.execute(
                "DELETE FROM docs WHERE entity_id=? AND doc_type=?",
                (eid, doc_type),
            )
            cx.execute(
                "INSERT INTO history(entity_id, event_type, payload) VALUES(?,?,?)",
                (eid, "doc:unset", json.dumps({"doc_type": doc_type})),
            )

