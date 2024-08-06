use rusqlite::{Connection, ToSql};
use std::fmt;
use std::fs;
use std::io;

enum MyError {
    Rusqlite(rusqlite::Error),
    Filesystem(io::Error),
    Csv(csv::Error),
}

impl fmt::Display for MyError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            MyError::Rusqlite(rusqlite) => write!(f, "{}", rusqlite),
            MyError::Filesystem(io) => write!(f, "{}", io),
            MyError::Csv(csv) => write!(f, "{}", csv),
        }
    }
}

impl From<rusqlite::Error> for MyError {
    fn from(err: rusqlite::Error) -> Self {
        MyError::Rusqlite(err)
    }
}

impl From<io::Error> for MyError {
    fn from(err: io::Error) -> Self {
        MyError::Filesystem(err)
    }
}

impl From<csv::Error> for MyError {
    fn from(err: csv::Error) -> Self {
        MyError::Csv(err)
    }
}

fn main() -> Result<(), String> {
    main_2().map_err(|e| e.to_string())
}

fn main_2() -> Result<(), MyError> {
    let _ = fs::remove_file("../feeds.sqlite");
    let conn = Connection::open("../feeds.sqlite")?;

    let creation_sql = fs::read_to_string("../src/structure.sql")?;

    conn.execute_batch(&creation_sql)?;

    for entry in fs::read_dir("../feeds")? {
        let path = entry?.path();
        let feed_name = path
            .file_stem()
            .expect(&format!("Invalid filename: {:?}", path))
            .to_string_lossy()
            .to_string();
        println!("Loading feed {feed_name}");
        if path.is_dir() {
            conn.execute("PRAGMA defer_foreign_keys = ON;", ())?;
            conn.execute("BEGIN", ())?;
            for table in fs::read_dir(path)? {
                let table_path = table?.path();
                if let Some(extension) = table_path.extension() {
                    if extension.to_string_lossy().to_string() != "txt" {
                        continue;
                    }
                } else {
                    continue;
                }

                let table_name = table_path
                    .file_stem()
                    .expect(&format!("Invalid filename: {:?}", table_path))
                    .to_string_lossy()
                    .to_string();

                if table_name == "shapes" {
                    continue;
                }

                println!("  Loading table {table_name}");

                let mut reader = csv::Reader::from_path(table_path)?;
                let headers = reader.headers()?;
                let columns = headers.into_iter().collect::<Vec<_>>().join(", ");
                let values = headers
                    .into_iter()
                    .map(|name| format!(":{name}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                let mut statement = conn.prepare(&format!(
                    "INSERT INTO {table_name} (feed, {columns}) VALUES ('{feed_name}', {values})"
                ))?;
                for row in reader.into_records() {
                    statement.execute(rusqlite::params_from_iter(row?.into_iter().map(|v| {
                        if v.is_empty() {
                            None
                        } else {
                            Some(v)
                        }
                    })))?;
                }
            }
            println!("Feed loaded, committing");
            conn.execute("COMMIT", ())?;
        }
    }

    Ok(())
}
