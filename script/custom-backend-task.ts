import sqlite3 from "sqlite3";
import { promisify } from "node:util";
import * as csvParse from "csv-parse";
import fs from "fs";
import path from "path";

export async function sqlite_open(filename: string): Promise<sqlite3.Database> {
    const db = new sqlite3.Database(filename);
    await new Promise<{}>((resolve, reject) => {
        const error = function (err: Error) {
            db.off("open", success);
            db.off("error", error);
            reject(err);
        };
        const success = function () {
            db.off("open", success);
            db.off("error", error);
            resolve({});
        };
        db.on("error", error);
        db.on("open", success);
    });
    await sqlite_run({
        db,
        statement: "PRAGMA foreign_keys = ON;",
        params: [],
    });
    return db;
}

export async function sqlite_close(db: sqlite3.Database): Promise<{}> {
    const close = promisify((callback: (err: Error | null) => void) =>
        db.close((err) => callback(err))
    );
    await close();
    return {};
}

export async function sqlite_serialize({
    db,
    statements,
}: {
    db: sqlite3.Database;
    statements: { statement: string; params: any }[];
}): Promise<{}> {
    const promises: Promise<sqlite3.RunResult>[] = [];
    db.serialize(() => {
        for (const { statement, params } of statements) {
            promises.push(sqlite_run({ db, statement, params }));
        }
    });
    await Promise.all(promises);
    return {};
}

export async function sqlite_run({
    db,
    statement,
    params,
}: {
    db: sqlite3.Database;
    statement: string;
    params?: any;
}): Promise<sqlite3.RunResult> {
    const run = promisify(
        (
            { statement, params }: { statement: string; params?: any },
            callback: (err: Error | null, result: sqlite3.RunResult) => void
        ) =>
            db.run(statement, params ?? [], function (err: Error | null) {
                callback(err, this);
            })
    );
    return await run({ statement, params });
}

export async function sqlite_load_csv({
    db,
    dir,
    feed,
    filename,
    table,
}: {
    db: sqlite3.Database;
    dir: string;
    feed: string;
    filename: string;
    table: string;
}): Promise<{}> {
    const fullPath = path.join(dir, feed, filename);
    try {
        await promisify(fs.stat)(fullPath);
    } catch (e) {
        if (e.code == "ENOENT") {
            // File does not exist
            return {};
        }
        throw e;
    }

    await sqlite_run({
        db,
        statement: "PRAGMA defer_foreign_keys = ON;",
    });
    await sqlite_run({ db, statement: "BEGIN" });
    const stream = fs.createReadStream(fullPath).pipe(
        csvParse.parse({
            columns: true,
            cast: (input) => {
                if (input.length == 0) {
                    return null;
                }
                const asNumber = parseFloat(input);
                if (isNaN(asNumber)) {
                    return input;
                } else {
                    return asNumber;
                }
            },
        })
    );

    const promises: Promise<sqlite3.RunResult>[] = [];
    for await (const line of stream) {
        const withDollar = { $feed: feed };
        for (const key of Object.keys(line)) {
            withDollar["$" + key] = line[key];
        }
        promises.push(
            sqlite_run({
                db: db,
                statement:
                    "INSERT INTO " +
                    table +
                    " (feed, " +
                    Object.keys(line).join(", ") +
                    ") VALUES (" +
                    Object.keys(withDollar).join(", ") +
                    ")",
                params: withDollar,
            })
        );
    }
    await Promise.all(promises);
    await sqlite_run({ db, statement: "COMMIT" });

    return {};
}
