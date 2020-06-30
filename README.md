# Sample application for Specifying a custom CCSID for a CICS service

This repository contains a sample CICS COBOL application that uses a custom code page when creating a CICS service for z/OS Connect EE. 

## Introduction
When you create a CICS service, you must specify the coded character set identifier (CCSID) to encode character data in COMMAREA and container application data structures at run time. In most cases, you can refer to Coded character set identifiers for the CCSIDs that are supported in z/OS Connect EE by default. But what if you want to use a custom code page that is not in the official CCSID list? Is it supported in z/OS Connect EE? Where should you start?

Here are the answers!

As of V3.0.32, z/OS Connect EE is enhanced to allow you to use a custom code page when creating a CICS service. By taking several prerequisite steps, you can now specify a custom CCSID in z/OS Connect EE. These steps are used to extend the Java Runtime Environment (JRE) to support your own custom character encoding with your own CharsetProvider and Charset classes.

## Prerequisites
- z/OS Connect Enterprise Edition is installed and a z/OS Connect EE instance has been created and configured with the CICS Service Provider and the API Requester function.
- CICS Transaction Server v5.2 or later.
- The CICS region is running and configured to access z/OS Connect EE to call APIs.
- Enterprise COBOL Compiler.
- Refer to [Coded character set identifiers](https://www-03preprod.ibm.com/support/knowledgecenter/SS4SVW_E29022/designing/ccsid_list.html) to check whether the code page that you want to use is supported in z/OS® Connect EE by default. Specify a custom CCSID only when you have a specific requirement for it.

## Define and Install CICS Resources 
- Clone the repository ``` git clone git@github.com:zhenzhenclaire zosconnect-usernamed-codepage.git```
- Upload the following source to your z/OS system and store on the PDS.
```
UCDZCEE.cbl
UCDCPY.cpy
```

- Customize your compile job and submit to compile the sample CICS COBOL program. The load module should be installed on a PDSE library that is accessible to the CICS region. The expected return code is 0.

## Configuring z/OS Connect EE Resources
- Create the following directories (if not done yet) called **resources/zosconnect/services** and **resources/zosconnect/apis** under your server path.

```shell
/var/zosconnect/servers/<server-name>/resources/zosconnect/services
/var/zosconnect/servers/<server-name>/resources/zosconnect/apis
```

- Recursively chown and chmod the output directories so your z/OS Connect server ID has access to them.
```shell
cd /var/zosconnect/servers/<server-name>
chown -R <serverID>:<serverGroup> ./resources
chmod -R 750 ./resources
```

- Setup definitions for the CICS IPIC connection in server.xml. The server.xml should have the following entries added.
```shell
<zosconnect_cicsIpicConnection id="zconipic"
	 host="<hostname>"
	 port="<portnum>"/>
```

**NOTE**: Change the hostname and portnum to the actual hostname and port number of the CICS region where the CICS sample program (CLAIMCI0) was installed. A sample server.xml is included in the package. If you want to use the sample server.xml file then upload it to z/OS in binary mode to keep the contents in ASCII format.

## Extending JRE to support custom character encoding
If you want to specify your custom CCSID in z/OS Conenct EE, you must extend the Java™ Runtime Environment (JRE) to support your custom character encoding with your own CharsetProvider and Charset classes. 

Sample CharsetProvider and Charset class implementations(```NHCCharsetProvider.java```, ```NHCCharset.java```) are included in the package. 
- ```NHCCharsetProvider.java``` demonstrates how to initialize a new character set "Cp99999". The numeric part "99999" is used to specify the CCSID.
- ```NHCCharset.java``` demonstrates how to transforms '垚' ('\uCE5D' in big5) to '孝'('\u5B5D' in unicode).

If you want to build your own custom CharsetProvider JAR file, follow the steps described in the [How to specify a custom CCSID for a CICS service](https://www-03preprod.ibm.com/support/knowledgecenter/SS4SVW_E29022/designing/specify_custom_ccsid.html) section of the z/OS Connect EE documentation in the IBM Knowledge Center. 

## Deploying the custom CharsetProvideer JAR file to JRE
A sample JAR file supporting CCSID(99999) is provided in this scenario. 
1. Copy the JAR file(```zosconnectUsernamedCodepage.jar```) to the z/OS Connect EE runtime JRE extend directory, for example, ```$ZCEERUNTIME_JRE_Location/jre/lib/ext```.
2. Depending on which toolkit that you use to generate the service archive (.sar), copy the JAR file to the corresponding JRE extend directory.
    - If you use the API toolkit, ensure the JAR file is deployed to the API toolkit JRE extend directory, for example, `$ZOSExplorer_Install_Location/jdk/jre/lib/ext`.
    - If you use the build toolkit, ensure the JAR file is deployed to the build toolkit JRE extend directory, for example, `$BUILDTOOLKIT_JRE_Location/jre/lib/ext`.

## Generating and Deploying .sar file to z/OS Connect EE Server
This repository includes the sample API (```CodePageAPIExample.aar```) and service (```CodePageExample.sar```) archive files.
Follow the steps below to deploy the included archive files:
- To deploy the sample service (```CodePageExample.sar```), follow the steps described in the Automated service archive management section of the z/OS Connect EE documentation in the IBM Knowledge Center. If transferring the file via ftp, ensure the file is transferred as binary.
- To deploy the sample API (```CodePageAPIExample.aar```), follow the steps described in the Automated API management section of the z/OS Connect EE documentation in the IBM Knowledge Center. If transferring the file via ftp, ensure the file is transferred as binary.

Follow the steps below to generate and deploy from the sample projects / source provided:
- On your IBM Explorer for z/OS (or any of the supported Eclipse environment), click on File -> Import then click on General -> Existing Projects into Workspace. Select the ```CICSClaimsServiceProject.zip``` file included in the package (confirm that the CICSClaimsService project is selected under the Projects field) and click Finish. Repeat the same steps for ```CICSClaimsAPIProject.zip``` (confirm that the CICSClaimsAPI project is selected under the Projects field).

- To deploy the service from the API Toolkit, follow the steps described in the Deploying a service section of the z/OS Connect EE documentation in the IBM Knowledge Center.

- To deploy the API from the API Toolkit, follow the steps described in the Deploying an API in the API toolkit section of the z/OS Connect EE documentation in the IBM Knowledge Center.

## Testing the sample API
At this point, the sample API is now ready for testing. Start by testing the REST API that is called from the CICS application.

To test the API using curl, type the following:
```
curl -X POST --header ‘Content-Type: application/json’ --header ‘Accept: application/json’ -d ‘{“CODEPAGE01”:{“QUERY_CUST_NAME”:“好垚小“}}’ ‘http://winmvs3s.hursley.ibm.com:10354/codepageapiexample/service’
```

Below is the sample output. Note that '垚' is shown by '孝'.
```
{
    “CODEPAGE01”: {
        “QUERY_CUST_NAME”: “好孝小”
    }
}
```

Below is the HEX value of CUST NAME shown in CICS. Note that 'CE5D' is '垚'.
![hex](https://raw.githubusercontent.com/zhenzhenclaire/zosconnect-usernamed-codepage/master/media/HEX.png)
## Conclusion

## Notice 

## License 


 For more information about the steps,
see .
