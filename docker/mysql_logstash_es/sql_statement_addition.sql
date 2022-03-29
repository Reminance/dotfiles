SELECT
    id, name, DATE_FORMAT(update_time,'%Y-%m-%d %H:%i:%s') update_time
FROM user
WHERE
	update_time >= DATE_SUB(:sql_last_value, INTERVAL 15 SECOND)
ORDER BY
	update_time ASC
