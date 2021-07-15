{
  Developer: Halil Han BADEM.
  Github: https://github.com/halilhanbadem
  Release Date: 15/07/2021
  Note; It is free and exchangeable. Please, if you are going to use the codes in an article, I would like you to cite the source.
  For collaborations: halilhanbadem@gmail.com
  Website: halilhanbadem.dev
}

unit hbOAuth;

interface

uses
 System.Hash,
 System.NetEncoding,
 System.SysUtils,
 System.Classes,
 System.StrUtils,
 System.DateUtils;

type
  TRequestMethod = (GET, POST, PUT, DEL, PATCH);
  TSignatureMethodVersion = (HMAC_SHA1, HMAC_SHA256);
  TOtherParameterPlace = (pFirst, pLast);
  TAfterCreateParameter = procedure(Sender: TObject; Params: String) of object;
  TAfterCreateRequestLink = procedure(Sender: TObject; RequestLink: string) of object;
  TAfterAutoTimeStamp = procedure(Sender: TObject; dTimeStamp: Integer) of object;
  TAfterAutoNonce = procedure(Sender: TObject; dNonce: string) of object;


type
  TOAuthParameters = class(TPersistent)
    private
     FToken: String;
     FTokenSecret: String;
     FNonce: String;
     FConsumerKey: String;
     FSignatureMethod: TSignatureMethodVersion;
     FVersion: String;
     FSecretKey: String;
     FTimeStamp: Integer;
     FOnChange: TNotifyEvent;
     procedure Changed;
     procedure setToken(AValue: String);
     procedure setTokenSecret(AValue: String);
     procedure setNonce(AValue: String);
     procedure setConsumerKey(AValue: String);
     procedure setSignatureMethod(AValue: TSignatureMethodVersion);
     procedure setSecretKey(AValue: String);
     procedure setTimeStamp(AValue: Integer);
    public
     procedure Assign(Source: TPersistent); override;
     property OnChange: TNotifyEvent read FOnChange write FOnChange;
    published
     property OAuthToken: String read FToken write setToken;
     property OAuthTokenSecret: String read FTokenSecret write setTokenSecret;
     property OAuthConsumerKey: String read FConsumerKey write setConsumerKey;
     property OAuthVersion: String read FVersion;
     property OAuthNonce: String read FNonce write setNonce;
     property OAuthSignatureMethod: TSignatureMethodVersion read FSignatureMethod write setSignatureMethod default HMAC_SHA1;
     property OAuthSecretKey: String read FSecretKey write setSecretKey;
     property OAuthTimeStamp: Integer read FTimeStamp write setTimeStamp default 0;
  end;

type
 ThbOAuth = class(TComponent)
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
   private
    HMAC_SHA1: THashSHA1;
    HMAC_SHA256: THashSHA2;
    FAfterCreateParameter: TAfterCreateParameter;
    FAfterCreateRequestLink: TAfterCreateRequestLink;
    FAfterAutoTimeStamp: TAfterAutoTimeStamp;
    FAfterAutoNonce: TAfterAutoNonce;
    FOAuthParameters: TOAuthParameters;
    FAutoNonce, FAutoTimeStamp: Boolean;
    FHost: String;
    FOtherParameter: string;
    FRequestMethod: TRequestMethod;
    FOtherParameterPlace: TOtherParameterPlace;
    StrSignMethod, StrGlobalParam, StrGlobalSignature: String;
    procedure setOAuthParameters(AValue: TOAuthParameters);
    procedure setParameterOrder(out strBaseParam: string);
    procedure setMethodStr(out strRequest, strHMAC: string);
    function  LastWord(Str: string): string;
   protected
    procedure dAfterCreateParameter(Params: String); dynamic;
    procedure dAfterCreateRequestLink(RequestLink: String); dynamic;
    procedure dAfterAutoTimeStamp(dTimeStamp: Integer); dynamic;
    procedure dAfterAutoNonce(dNonce: String); dynamic;
   public
    function GenerateTimeStamp(Minute: Integer = 1): Integer;
    function GenerateNonce(NonceLength: Integer = 32): String;
    function CreateSignature(Encoded: Boolean = False): String;
    function CreateRequestLink: String;
   published
    property AfterCreateParameter: TAfterCreateParameter read FAfterCreateParameter write FAfterCreateParameter;
    property AfterCreateRequestLink: TAfterCreateRequestLink read FAfterCreateRequestLink write FAfterCreateRequestLink;
    property AfterAutoTimeStamp: TAfterAutoTimeStamp read FAfterAutoTimeStamp write FAfterAutoTimeStamp;
    property AfterAutoNonce: TAfterAutoNonce read FAfterAutoNonce write FAfterAutoNonce;

    property OAuthParameters: TOAuthParameters read FOAuthParameters write setOAuthParameters;
    property AutoGenerateNonce: Boolean read FAutoNonce write FAutoNonce default false;
    property AutoGenerateTimestamp: Boolean read FAutoTimeStamp write FAutoTimeStamp default false;
    property Host: string read FHost write FHost;
    property RequestMethod: TRequestMethod read FRequestMethod write FRequestMethod default GET;
    property OtherParameters: string read FOtherParameter write FOtherParameter;
    property OtherParameterPlace: TOtherParameterPlace read FOtherParameterPlace write FOtherParameterPlace default pFirst;
 end;
 procedure Register;


implementation

{ThbOAuth}

procedure Register;
begin
  RegisterComponents('hbComponentSet', [ThbOAuth]);
end;

constructor ThbOAuth.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  HMAC_SHA1 := THashSHA1.Create;
  HMAC_SHA256 := THashSHA2.Create;
  FOAuthParameters := TOAuthParameters.Create;
  FOAuthParameters.FVersion := '1.0';
end;

destructor ThbOAuth.Destroy;
begin
 FOAuthParameters.Free;
 inherited;
end;

procedure ThbOAuth.dAfterCreateParameter(Params: String);
begin
 if Assigned(FAfterCreateParameter) then
 begin
   FAfterCreateParameter(Self, Params);
 end;
end;

procedure ThbOAuth.dAfterCreateRequestLink(RequestLink: String);
begin
 if Assigned(FAfterCreateRequestLink) then
 begin
   FAfterCreateRequestLink(Self, RequestLink);
 end;
end;

procedure ThbOAuth.dAfterAutoTimeStamp(dTimeStamp: Integer);
begin
 if Assigned(FAfterAutoTimeStamp) then
 begin
   FAfterAutoTimeStamp(Self, dTimeStamp);
 end;
end;

procedure ThbOAuth.dAfterAutoNonce(dNonce: String);
begin
 if Assigned(FAfterAutoNonce) then
 begin
   FAfterAutoNonce(Self, dNonce);
 end;
end;

function ThbOAuth.LastWord(Str: string): string;
begin
 Result := Copy(Str, Length(Str), 1);
end;

procedure ThbOAuth.setMethodStr(out strRequest, strHMAC: string);
begin
 case FRequestMethod of
   GET: strRequest := 'GET';
   POST: strRequest := 'POST';
   PUT: strRequest := 'PUT';
   DEL: strRequest := 'DELETE';
   PATCH: strRequest := 'PATCH';
 end;

 case FOAuthParameters.FSignatureMethod of
   TSignatureMethodVersion.HMAC_SHA1: strHMAC := 'HMAC-SHA1';
   TSignatureMethodVersion.HMAC_SHA256: strHMAC := 'HMAC-SHA256';
 end;
end;

procedure ThbOAuth.setParameterOrder(out strBaseParam: String);
begin
 strBaseParam := 'oauth_consumer_key=' + FOAuthParameters.FConsumerKey;

 if (Trim(FOAuthParameters.FNonce) <> '') then
 begin
  strBaseParam := strBaseParam + '&oauth_nonce=' + FOAuthParameters.FNonce;
 end;

 strBaseParam := strBaseParam + '&oauth_signature_method=' + StrSignMethod;
 strBaseParam := strBaseParam + '&oauth_timestamp=' + FOAuthParameters.FTimeStamp.ToString;

 if (Trim(FOAuthParameters.FToken) <> '') then
 begin
  strBaseParam := strBaseParam + '&oauth_token=' + FOAuthParameters.FToken;
 end;

 strBaseParam := strBaseParam + '&oauth_version=' + FOAuthParameters.FVersion;

 if (Trim(FOtherParameter) <> '') then
 begin
   if (FOtherParameterPlace = pFirst) then
   begin
     strBaseParam :=  FOtherParameter + '&' + strBaseParam;
   end else
   begin
     strBaseParam := strBaseParam + '&' + FOtherParameter;
   end;
 end;
end;

function ThbOAuth.CreateSignature(Encoded: Boolean): String;
var
 BaseText, Request, SecretTokenKey, BaseParams : String;
begin
 BaseText       := '';
 Request        := '';
 SecretTokenKey := '';
 BaseParams     := '';

 if (FAutoNonce) then
 begin
   FOAuthParameters.FNonce :=GenerateNonce(11);
   dAfterAutoNonce(FOAuthParameters.FNonce);
 end;

 if (FAutoTimeStamp) then
 begin
   FOAuthParameters.FTimeStamp := GenerateTimeStamp(1);
   dAfterAutoTimeStamp(FOAuthParameters.FTimeStamp);
 end;

 setMethodStr(Request, StrSignMethod);


 if (LastWord(FOtherParameter) = '&') then
 begin
  FOtherParameter := Copy(FOtherParameter, 0, Length(FOtherParameter) - 1);
 end;

 BaseText := Request + '&' + TNetEncoding.URL.Encode(FHost) + '&';
 SecretTokenKey:= TNetEncoding.URL.Encode(FOAuthParameters.FSecretKey) + '&' + TNetEncoding.URL.Encode(FOAuthParameters.FTokenSecret);
 setParameterOrder(BaseParams);
 dAfterCreateParameter(BaseParams);
 StrGlobalParam := BaseParams;
 BaseParams := TNetEncoding.URL.Encode(BaseParams);
 BaseText := BaseText + BaseParams;

 if (FOAuthParameters.FSignatureMethod = TSignatureMethodVersion.HMAC_SHA1) then
 begin
  if Encoded then
  begin
   Result := TNetEncoding.URL.Encode(TNetEncoding.Base64.EncodeBytesToString(HMAC_SHA1.GetHMACAsBytes(TEncoding.UTF8.GetBytes(BaseText), TEncoding.UTF8.GetBytes(SecretTokenKey))));
  end else
  begin
   Result := TNetEncoding.Base64.EncodeBytesToString(HMAC_SHA1.GetHMACAsBytes(TEncoding.UTF8.GetBytes(BaseText), TEncoding.UTF8.GetBytes(SecretTokenKey)));
  end;
  StrGlobalSignature := Result;
 end;

 if (FOAuthParameters.FSignatureMethod = TSignatureMethodVersion.HMAC_SHA256) then
 begin
  if Encoded then
  begin
   Result := TNetEncoding.URL.Encode(TNetEncoding.Base64.EncodeBytesToString(HMAC_SHA256.GetHMACAsBytes(TEncoding.UTF8.GetBytes(BaseText), TEncoding.UTF8.GetBytes(SecretTokenKey))));
  end else
  begin
   Result := TNetEncoding.Base64.EncodeBytesToString(HMAC_SHA256.GetHMACAsBytes(TEncoding.UTF8.GetBytes(BaseText), TEncoding.UTF8.GetBytes(SecretTokenKey)));
  end;
  StrGlobalSignature := Result;
 end;
end;

function ThbOAuth.CreateRequestLink: String;
var
 BeforeResult: string;
begin
 BeforeResult := FHost + '?' + StrGlobalParam + '&oauth_signature=' + StrGlobalSignature;
 dAfterCreateRequestLink(BeforeResult);
 Result := BeforeResult;
end;

procedure ThbOAuth.setOAuthParameters(AValue: TOAuthParameters);
begin
 FOAuthParameters.Assign(AValue);
end;

function ThbOAuth.GenerateTimeStamp(Minute: Integer): Integer;
begin
 if Minute <= 0 then
 begin
   Minute := 1;
 end;

 Result := DateTimeToUnix(TTimeZone.Local.ToUniversalTime(IncMinute(Now, Minute)));
end;

function ThbOAuth.GenerateNonce(NonceLength: Integer): String;
const
 Words: String = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890';
var
 TotalWordsLength, I: Integer;
begin
 if NonceLength <= 0 then
 begin
   NonceLength := 32;
 end;

 TotalWordsLength := Length(Words);
 SetLength(Result, NonceLength);

 for I := 1 to NonceLength do
 begin
   Result[I] := Words[Random(TotalWordsLength) + 1];
 end;
end;

{TOAuthParameters}

procedure TOAuthParameters.Changed;
begin
 if Assigned(FOnChange) then
 begin
   FOnChange(Self);
 end;
end;

procedure TOAuthParameters.Assign(Source: TPersistent);
begin
 if Source is TOAuthParameters then
 begin
   FToken           :=  TOAuthParameters(Source).FToken;
   FNonce           :=  TOAuthParameters(Source).FNonce;
   FConsumerKey     :=  TOAuthParameters(Source).FConsumerKey;
   FSignatureMethod :=  TOAuthParameters(Source).FSignatureMethod;
   FVersion         :=  TOAuthParameters(Source).FVersion;
   FSecretKey       :=  TOAuthParameters(Source).FSecretKey;
   FTimeStamp       :=  TOAuthParameters(Source).FTimeStamp;
 end else
     begin
       inherited;
     end;
end;

procedure TOAuthParameters.setToken(AValue: String);
begin
 if FToken <> AValue then
 begin
   FToken := AValue;
   Changed;
 end;
end;

procedure TOAuthParameters.setTokenSecret(AValue: String);
begin
 if FTokenSecret <> AValue then
 begin
   FTokenSecret := AValue;
   Changed;
 end;
end;

procedure TOAuthParameters.setNonce(AValue: String);
begin
 if FNonce <> AValue then
 begin
   FNonce := AValue;
   Changed;
 end;
end;

procedure TOAuthParameters.setConsumerKey(AValue: String);
begin
 if FConsumerKey <> AValue then
 begin
   FConsumerKey := AValue;
   Changed;
 end;
end;

procedure TOAuthParameters.setSignatureMethod(AValue: TSignatureMethodVersion);
begin
 if FSignatureMethod <> AValue then
 begin
   FSignatureMethod := AValue;
   Changed;
 end;
end;

procedure TOAuthParameters.setSecretKey(AValue: String);
begin
 if FSecretKey <> AValue then
 begin
   FSecretKey := AValue;
   Changed;
 end;
end;

procedure TOAuthParameters.setTimeStamp(AValue: Integer);
begin
 if FTimeStamp <> AValue then
 begin
   FTimeStamp := AValue;
   Changed;
 end;
end;

end.
