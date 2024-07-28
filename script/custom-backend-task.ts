import sqlite3 from "sqlite3";
import { promisify } from "node:util";

export async function sqlite_open(filename: string): Promise<sqlite3.Database> {
    const db = new sqlite3.Database(filename);
    return await new Promise<sqlite3.Database>((resolve, reject) => {
        const error = function () {
            db.off("open", success);
            db.off("error", error);
            reject();
        };
        const success = function () {
            db.off("open", success);
            db.off("error", error);
            resolve(db);
        };
        db.on("error", error);
        db.on("open", success);
    });
}

export async function sqlite_close(db: sqlite3.Database): Promise<{}> {
    const close = promisify((callback) =>
        db.close((err) => callback(err, null))
    );
    await close();
    return {};
}

export async function sqlite_serialize({
    db,
    statements,
}: {
    db: sqlite3.Database;
    statements: string[];
}): Promise<{}> {
    const run = promisify(
        (statement: string, callback: (err: Error | null) => void) =>
            db.run(statement, [], callback)
    );
    const promises: Promise<void>[] = [];
    db.serialize(() => {
        for (const statement of statements) {
            console.info(statement);
            promises.push(run(statement));
        }
    });
    await Promise.all(promises);
    return {};
}
