// 获取当前环境的名称
const currentEnvironment = pm.environment['name']

// 打印当前环境的名称
console.log('当前环境名称:', currentEnvironment);
 
// 检查当前环境是否为 "test"
if (currentEnvironment === "test") {
    // 如果是 "test" 环境，执行预请求脚本的逻辑
    console.log("当前环境是 test，启用预请求脚本的逻辑。");
    // 这里可以添加你需要的逻辑代码
    var request_path = pm.request.url.getPath();                // 请求路径（不包含参数）
    var sys_name = "test";                   // 系统名称
    var private_key = ''; //秘钥
    pre_request(request_path,sys_name,private_key)
} else {
    // 如果不是 "test" 环境，可以添加其他逻辑或者什么都不做
    console.log("当前环境不是 test，不启动预请求脚本的逻辑。");
}

/*
 请求开始之前计算签名等，并放入到postman全局变量中。
 在postman真正发送请求的时候会引用到我们提前计算好的结果
*/
function pre_request(request_path,sys_name,private_key){
   var timestamp = Math.round(new Date().getTime());
    var sign_string = generate_sign_string(sys_name,timestamp,request_path);
    var signature = compute_signature(request_path, sys_name, private_key,timestamp,sign_string)
    
    set_global_var(signature,timestamp,sys_name,request_path)
    set_request_header(signature,timestamp,sys_name)
}

/*
  将计算好的签名等放到postman全局变量中
*/
function set_global_var(signature,timestamp,sys_name,request_path){
	postman.setGlobalVariable("x-lt-signature", signature);
	postman.setGlobalVariable("x-lt-timestamp", timestamp);
	postman.setGlobalVariable("x-lt-openKeyId", sys_name);
	postman.setGlobalVariable("request_path", request_path);
}

/*
  添加请求头
*/
function set_request_header(signature,timestamp,sys_name){
	// 在脚本中设置请求头
  pm.request.headers.add({
    key: "x-lt-openKeyId",
    value: sys_name
  });
  pm.request.headers.add({
    key: "x-lt-timestamp",
    value: timestamp
  });
  pm.request.headers.add({
    key: "x-lt-signature",
    value: signature
  });
}

/*
  产生随机字符串
  len:字符串的长度
*/
function generate_random_string(len) {
　　var chars = 'ABCDEFGHJKMNPQRSTWXYZabcdefhijkmnprstwxyz2345678';
　　var maxPos = chars.length;
　　var pwd = '';
　　for (var i = 0; i < len; i++) {
　　　　pwd += chars.charAt(Math.floor(Math.random() * maxPos));
　　}
　　return pwd;
} 


/*
  产生签名字符串
  计算公式 ：SignString = openKeyId+ "&"  + TIMESTAMP + "&" + REQUEST_PATH
*/
function generate_sign_string(openKeyId ,timestamp ,request_path){
	return openKeyId + "&" + timestamp + "&" + request_path
}

/*
  计算签名 
  
  计算公式 Signature =  = RANDOM_KEY + BASE64(HMAC-SHA256(SignString，SecretKey))
  其中SecretKey = PRIVATE_KEY + RANDOM_KEY
  
  request_path 请求路径
  sys_name     系统名称
  private_key  私钥
  timestamp    当前时间戳
*/
function compute_signature(request_path, sys_name, private_key,timestamp,sign_string){
    var private_keys = private_key.split(",");
    console.log(private_keys.length);
	var random_key  = generate_random_string(5);
	var final_signature = "";
	for (var i = 0; i < private_keys.length; i++) {		
		var secret_key = private_keys[i] + random_key;
		var raw = CryptoJS.HmacSHA256(sign_string,secret_key).toString();
		var words = CryptoJS.enc.Utf8.parse(raw); 
		var base64 = CryptoJS.enc.Base64.stringify(words); 
		final_signature = random_key + base64 ;
		if (i < private_keys.length -1){
			final_signature = final_signature + ",";
		}
    }
	return final_signature;
}

