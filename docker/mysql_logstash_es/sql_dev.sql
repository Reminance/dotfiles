select acr.id, acr.check_code, acr.check_type, acr.check_task_code, acr.check_way, acr.is_diff, acr.warehouse_id,
    acr.sub_warehouse_id, acr.`floor`, acr.area_id, acr.roadway, acr.`location`, acr.pick_order, acr.goods_sn, acr.`size`,
    acr.system_number, acr.check_number, acr.diff_number, acr.check_user, acr.`sequence`, acr.is_delete, acr.create_time,
    acr.last_update_time, acr.vmi_num, acr.un_vmi_num, task.task_status,acr.sku_code, i.store_type_id
    from at_check_result as acr
    inner join at_check_task task on acr.check_task_code = task.check_task_code
    inner join inf_goods i on acr.sku_code = i.sku_code
WHERE
    acr.last_update_time >= DATE_SUB(:sql_last_value, INTERVAL 1 SECOND)
ORDER BY
    acr.last_update_time ASC
