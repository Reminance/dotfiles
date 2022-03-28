SELECT
    id, name, DATE_FORMAT(update_time,'%Y-%m-%d %T') update_time
FROM user
WHERE
	update_time >= DATE_SUB(:sql_last_value, INTERVAL 5 MINUTE)
ORDER BY
	update_time ASC
