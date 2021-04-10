rem .\test.bat biz-eureka biz-config biz-gateway biz-auth biz-trading biz-oa biz-user biz-schedule biz-promotion biz-supplier
@echo off
for %%a in (%*) do (
	set /a num+=1
	if "%%a"=="biz-eureka" (
		call :build-base %%a
	) else if "%%a"=="biz-config" (
		call :build-base %%a
	) else if "%%a"=="biz-gateway" (
		call :build-base %%a
	) else if "%%a"=="biz-schedule" (
		call :build-base %%a
	) else (
		call :build-biz %%a
	)
)
if defined num (echo build %num% service complete) else echo USAGE: %0 erueka config api
echo closing in 3 seconds...
ping -n 3 127.1>nul
EXIT /B 1

:build-base
echo ---------------------
echo start building "%1"
rem call mvn -T 4C clean install -U -pl %1/  -am -amd clean package -Dmaven.test.skip=true
call mvn -T 4C clean install -pl %1/  -am -amd clean package -Dmaven.test.skip=true
rem > null
echo copying jar "%1"
call scp %cd%/%1/target/%1-0.0.1-SNAPSHOT.jar root@172.16.1.212:/opt/data/workstation/apps/
echo ---------------------
echo.
ping -n 2 127.1>nul
goto:eof

:build-biz
echo ---------------------
echo start building "%1"
rem call mvn -T 4C clean install -U -pl biz-business/%1/%1-api/  -am -amd clean package -Dmaven.test.skip=true
call mvn -T 4C clean install -pl biz-business/%1/%1-api/  -am -amd clean package -Dmaven.test.skip=true
rem > null
echo copying jar "%1"
call scp %cd%/biz-business/%1/%1-api/target/%1-api.jar root@172.16.1.212:/opt/data/workstation/apps/
echo ---------------------
echo.
ping -n 2 127.1>nul
goto:eof


