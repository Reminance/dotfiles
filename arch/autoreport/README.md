# 自动提交日报
## 如何使用
### 1.project-common.conf配置需要采集的项目以及分支信息
```
[
    {
        "project":"mall_bn_java",
        "branchs":[
            "dev-supplier",
            "dev-optimize-0628"
        ]

    },
    {
        "project":"queen-crm",
        "branchs":[
                "dev",
                "test"
        ]
    },
    {
        "project":"queen_pay",
        "branchs":[
            "dev_supplier"
        ]
    }
]
```
  start.sh里的"url"是提交地址(开发环境无法域名直连生产， 可使用http://172.16.1.27:8080/daily/api/v1作为转发)

### 2.配置user.conf
```
[
    {
        "user":"xucheng",
        "daily_user_id":23,
        "daily_user_name":"许诚",
        "daily_project":"供应商版本",
        "daily_action":"开发",
        "daily_status":"进行中",
        "projects":[
            {
                "project":"mall_bn_java",
                "branchs":[
                    "dev-supplier",
                    "dev-optimize-0628"
                ]
            }
        ]
    }
]
```

### 3.启动提交脚本 或者配置crontab
```
./start.sh
```
