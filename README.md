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


## hbGoogle ![google](https://user-images.githubusercontent.com/17130294/125825176-68acceb8-c38d-4c7b-9d15-69a7896c3898.png)


## hbMailSender ![mail](https://user-images.githubusercontent.com/17130294/125825452-983cdc44-ad8f-487f-87d6-1a3e679d19b7.png)

