#!/usr/bin/env python3

import pymysql
import config
import json
from datetime import datetime

# === 配置 ===
DB_CONFIG = config.DB_CONFIG
SQL_FILE = './modular_temp.sql'
DICT_MAPPING_FILE = './dict_mapping.json'

# === 工具函数 ===

def read_sql_file(file_path):
    with open(file_path, 'r', encoding='utf-8') as f:
        sql_content = f.read()
    return [stmt.strip() for stmt in sql_content.split(';') if stmt.strip().lower().startswith('create table')]

def table_exists(cursor, table_name):
    cursor.execute(f"SHOW TABLES LIKE '{table_name}'")
    return cursor.fetchone() is not None

def ensure_tables(cursor, sql_file_path):
    create_stmts = read_sql_file(sql_file_path)
    for stmt in create_stmts:
        table_name = stmt.split()[2].strip('`')
        if not table_exists(cursor, table_name):
            print(f"[建表] 创建表 {table_name}")
            cursor.execute(stmt)
        else:
            print(f"[跳过] 表 {table_name} 已存在")

def ensure_cat(cursor, cat_code, cat_name):
    cursor.execute("SELECT id FROM sys_dict_cat WHERE cat_code = %s", (cat_code,))
    row = cursor.fetchone()
    if row:
        print(f"[存在] 字典分类 {cat_code} 已存在")
        return row[0]
    else:
        now = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
        sql = """
        INSERT INTO sys_dict_cat (cat_code, cat_name, is_delete, create_time, last_update_time, is_translate)
        VALUES (%s, %s, 0, %s, %s, 1)
        """
        cursor.execute(sql, (cat_code, cat_name, now, now))
        print(f"[插入] 添加字典分类 {cat_code}")
        return cursor.lastrowid

def cat_has_dicts(cursor, cat_id):
    cursor.execute("SELECT COUNT(*) FROM sys_dict WHERE cat_id = %s", (cat_id,))
    return cursor.fetchone()[0] > 0

def insert_dict_values(cursor, cat_id, values):
    now = datetime.now().strftime('%Y-%m-%d %H:%M:%S')
    for code, name in values.items():
        sql = """
        INSERT INTO sys_dict (cat_id, dict_code, dict_name_zh, dict_name_en, is_delete, create_time, last_update_time)
        VALUES (%s, %s, %s, '', 0, %s, %s)
        """
        cursor.execute(sql, (cat_id, code, name, now, now))
        print(f"[字典项] 插入 {code} - {name}")

# === 主逻辑 ===

def main():
    with pymysql.connect(**DB_CONFIG) as conn:
        with conn.cursor() as cursor:
            ensure_tables(cursor, SQL_FILE)

            with open(DICT_MAPPING_FILE, 'r', encoding='utf-8') as f:
                dict_mapping = json.load(f)

            for cat_code, cat_info in dict_mapping.items():
                cat_name = cat_info['cat_name']
                values = cat_info.get('values', {})
                cat_id = ensure_cat(cursor, cat_code, cat_name)

                if not cat_has_dicts(cursor, cat_id) and values:
                    insert_dict_values(cursor, cat_id, values)

            conn.commit()
            print("\n✅ 所有操作完成。")

if __name__ == '__main__':
    main()

