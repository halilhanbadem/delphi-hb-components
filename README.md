<p align="center">
  <img src="https://user-images.githubusercontent.com/17130294/125825274-6a873d92-1806-4bee-88f9-5096aafc48c3.png">
</p>


# Welcome to Delphi H "Han" B Component Set
## What is an HB component set? What is it used for?
These components include the classes that I have prepared for my needs. There are actually many classes, but I thought they would be necessary for now so I turned them into a set of components. With this set of 3 components, you can use Google Speech to Text technology, send mail easily, and have the signature generator required for the OAuth 1.0 verification system. Do not forget that; Please report any issue on Github. I may soon include a component related to AES encryption in the kit. A little tip; this component will also be able to communicate with different languages. For example, between PHP and Delphi we may have two different results when we need to communicate with AES. My class is ready to make sure you don't get into this situation. I may add later. Without further ado, let me introduce the components in the set.

## hbOAuth ![oauth](https://user-images.githubusercontent.com/17130294/125825227-c7137288-9036-4423-b24c-33390e269896.png)

hbOAuth is a signature generation component. With this component, you can create your signature in accordance with the OAuth 1.0 system. It can create your signature according to HMAC_SHA1 and HMAC_SHA256 types. In addition, completely native classes and components are used. 

### Propertys
 -  **`AutoGenerateNonce`**: It automatically generates the value for the "Nonce" parameter. If true, even if you set it, it will take the value it created.
 -  **`AutoGenerateTimeStamp`**: It automatically generates the value for the "TimeStamp" parameter. If true, even if you set it, it will take the value it created.
 -  **`Host`**: The request asks for the host information you will send. Thus, it is required for Base Plain Text.
 -  **`OAuthParameters`**: Required parameters for OAuth.
    -  **`OAuthConsumerKey`**: OAuth consumer key field.
    -  **`OAuthNonce`**: OAuth nonce field.
    -  **`OAuthSecretKey`**: OAuth secret key field.
    -  **`OAuthSignatureMethod`**: OAuth signature method field. It covers the "HMAC-SHA1" or "HMAC-SHA256" option.
    -  **`OAuthTimeStamp`**: OAuth timestamp field.
    -  **`OAuthToken`**: OAuth token field.
    -  **`OAuthTokenSecret`**: OAuth token secret field.
    -  **`OAuthVersion`**: OAuth version field. It cannot be changed. It's just 1.0 as a constant.
 -  **`OtherParameterPlace`**: The property that detects whether the parameter is at the beginning or the end. Includes pFirst...pLast values.
 -  **`OtherParameters`**: If additional parameter is to be added, it should be added to this property.
 -  **`RequestMethod`**: The request type field. GET, POST etc. selection is made. Required for signature.

### Events
 - **`AfterAutoNonce`** It is triggered after the automatic nonce value is created.
 - **`AfterAutoTimeStamp`**: It is triggered after the automatic timestamp value is created.
 - **`AfterCreateParameter`**: It is triggered after the parameter value is created.
 - **`AfterCreateRequestLink`**: It is triggered after the request link value is created. 

### Functions
 - `CreateSignature(Encoded: Boolean = False): String;`: It just creates a signature. Before doing this, you need to set the relevant properties. Like the sample code below.
 - `CreateRequestLink: String;`: Returns the relevant request link. The CreateSignature function must be called before doing this.

```
procedure TForm1.Button1Click(Sender: TObject);
begin
 with hbOAuth1 do
 begin
   Host := 'http://localhost/wordpress/wp-json/wc/v3/orders';
   AutoGenerateNonce := True;
   AutoGenerateTimestamp := True;
   RequestMethod := GET;
   OAuthParameters.OAuthConsumerKey := 'ck_f3e717c1af35ca0a8be2e39b44022fa2f8d3973d';
   OAuthParameters.OAuthSignatureMethod := HMAC_SHA1;
   OAuthParameters.OAuthSecretKey := 'cs_93883db67027475426aff3b2da5e2e58aa464845';
   ShowMessage(CreateSignature);
   ShowMessage(CreateRequestLink); //optional.
 end;
end;
```
P.S: The usage area of ​​this component is quite wide. I did my tests in the OAuth system for the API in Wordpress's Woocommerce plugin.
P.S: I know about "OAuth1Authenticator", but it is designed for those who think like me and want to use Delphi's ready-made REST Client components.

### Resources
 - https://www.rfc-editor.org/rfc/rfc5849.html#section-3.4.1
 - https://oauth.net/core/1.0a/#RFC2045

## hbGoogle ![google](https://user-images.githubusercontent.com/17130294/125825176-68acceb8-c38d-4c7b-9d15-69a7896c3898.png)
hbGoogle is the component that allows you to use the Google Speech to Text API using Google Cloud technologies. [Those who want to have this component other than the set can visit this link.](https://github.com/halilhanbadem/delphi-google-speech-to-text)  Its main purpose is to send commands with voice and to process this command in your projects. In the example of this project, a specific file is processed. If you wish, you can record and process instant audio with 3rd party components.

## Requirements
 * Indy Components
 * ssleay32.dll and libeay32.dll

## Usage
 * Add one hbGoogle component to your project.
 * Add a button and come to the onclick event.
 * for hbGoogle; Define the PrivateKeyFile property value, the path to the json file we downloaded from Google.
 * For the VoiceFile property value, define the path to the sound file. Remember; The file should have wav extension and 16000hz.
 * If the audio file is not suitable, it will give you an error.
 * The SpeechLanguage value is for the language. Indicates which language the sound belongs to.
 * Very long files can be stuck at Google limits when converting to base64 format. For this, with the CloudStorage structure that I will consider in the future, I can first send the voices to the cloud and then convert them by giving uri.

Sample code:

```
hbGoogle1.SpeechLanguage := 'tr-TR';
hbGoogle1.PrivateKeyFile := 'delphisestenyazi.json';
hbGoogle1.VoiceFile := 'temp.wav';
ShowMessage(hbGoogle1.SpeechToText); 
```

That's it! The return will come back to you in writing.

## Some Warnings

 * Please make sure that the sound recording is 16000hz and its original wav extension. You can use FFMpeg.
 * Excessive or incorrect use may have occurred in the codes. I will fix these issues over time.. Since I am making open source codes as a hobby; I can be interested in my work whenever I find time.
 * Please create an issue for your problems.
 * It only supports VCL.
 * Developed and tested with Delphi 10.4.2.

## hbMailSender ![mail](https://user-images.githubusercontent.com/17130294/125825452-983cdc44-ad8f-487f-87d6-1a3e679d19b7.png)
hbMailSender comes from HHB Mail Component. [For that you can visit this link.](https://github.com/halilhanbadem/HHBMailComponent_Source) One of the biggest innovations is the ability to send HTML-based mails easily. You can use it without any problems, especially when sending a newsletter. In addition to having many features such as adding files, the code snippets that cause the MemoryLeak error have been corrected in this version. It uses Indy components.

_Update List_

**V1.1**

* Added "ConnectionTimeOut" to property section.
* Added file sending feature.
* ContentType, ContentID attributes added.
* By default, CharSet is set to "UTF-8".


**V1.2**

* Added ability to send mail to multiple users. [Click here for detailed usage.](https://github.com/halilhanbadem/HHBMailComponent_Source/issues/1)
* Added ability to send multiple files. [Click here for detailed usage.](https://github.com/halilhanbadem/HHBMailComponent_Source/issues/2)
* Bug fixed.

**V1.3**

* Bug fixed.

**V1.4**

* Removed the HTML requirement for whitespace adjustment.
* Version information was added to the properties section.

**V1.5**

* Added UniGUI support.

**V1.0 - hbcomponentset**

* Fixed MemoryLeak Error.
* Fixed some issues for HTML-based submissions.
* Minor code changes have been made.


## For Collaborations
 - halilhanbadem.dev
 - me@halilhanbadem.dev
 - halilhanbadem[at]gmail[dot].com

