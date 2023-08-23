{
  Developer: Halil Han BADEM.
  Github: https://github.com/halilhanbadem
  Release Date: 15/07/2021
  Note; It is free and exchangeable. Please, if you are going to use the codes in an article, I would like you to cite the source.
  For collaborations: halilhanbadem@gmail.com
  Website: halilhanbadem.dev
}

unit hbMailSender;

interface

uses
     System.SysUtils,
     System.Classes,
     IdSSLOpenSSL,
     IdMessage,
     IdExplicitTLSClientServerBase,
     IdSMTPBase,
     IdSMTP,
     IdAttachmentFile,
     IdText;

type
 ThbMailSender = class(TComponent)
     destructor Destroy; override;
     constructor Create(AOwner: TComponent); override;
   private
     FAuthor: String;
     FMail: String;
     FVersion: String;
     FSMTPHost: String;
     FSMTPPort: Word;
     FSMTPMailAddress: String;
     FSMTPMailPassword: String;
     FMailSubject: String;
     FClientMailAddress: String;
     FClientMailName: String;
     FMailReplyToAddress: String;
     FSMTPName: String;
     FContentID: String;
     FContentType: String;
     FMailContent: TStringList;
     FAttachFiles: TStringList;
     FAttachFilesType: TStringList;
     FAttachFilesID: TStringList;
     FConnectTimeOut: Integer;
     FAttachFileName: string;
     FAttachType: String;
     SMTPComponent: TIdSMTP;
     EMailComponent: TIdMessage;
     LHandlerComponent: TIdSSLIOHandlerSocketOpenSSL;
     procedure WLines(Value: TStringList);
     procedure WAttachFiles(Value: TStringList);
     procedure WAttachFilesType(Value: TStringList);
     procedure WAttachFilesID(Value: TStringList);
   public
     procedure Connect;
     procedure Disconnect;
     procedure SendMail;
     property AttachFiles: TStringList read FAttachFiles write WAttachFiles;
     property AttachFilesType: TStringList read FAttachFilesType write WAttachFilesType;
     property AttachFilesID: TStringList read FAttachFilesID write WAttachFilesID;
   published
    property AuthorName: String read FAuthor;
    property AuthorMailAddress: String read FMail;
    property VersionInfo: String read FVersion;
    property SMTPHost: String read FSMTPHost write FSMTPHost;
    property SMTPPort: Word read FSMTPPort write FSMTPPort;
    property SMTPMailAddress: String read FSMTPMailAddress write FSMTPMailAddress;
    property SMTPMailPassword: String read FSMTPMailPassword write FSMTPMailPassword;
    property MailSubject: String read FMailSubject write FMailSubject;
    property ClientMailAddress: String read FClientMailAddress write FClientMailAddress;
    property ClientMailName: String read FClientMailName write FClientMailName;
    property MailReplyToAddress: String read FMailReplyToAddress write FMailReplyToAddress;
    property MailContent: TStringList read FMailContent write WLines;
    property MailName: String read FSMTPName write FSMTPName;
    property ConnectionTimeOut: Integer read FConnectTimeOut write FConnectTimeOut;
    property AttachFile: string read FAttachFileName write FAttachFileName;
    property AttachType: string read FAttachType write FAttachType;
    property AttachFileContentID: string read FContentID write FContentID;
    property ContentType: string read FContentType write FContentType;
 End;
    Procedure Register;

implementation


{Made by HBBMailSend}

procedure Register;
begin
  RegisterComponents('hbComponentSet', [ThbMailSender]);
end;


constructor ThbMailSender.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAuthor := 'Halil Han BADEM';
  FMail := 'halilhanbadem@gmail.com';
  FVersion := 'V1.0';
  FMailContent := TStringList.Create;
  FAttachFiles := TStringList.Create;
  FAttachFilesType := TStringList.Create;
  FAttachFilesID := TStringList.Create;
  SMTPComponent := TIdSMTP.Create(nil);
  EMailComponent := TIdMessage.Create(nil);
  LHandlerComponent := TIdSSLIOHandlerSocketOpenSSL.Create(nil);
end;

destructor ThbMailSender.Destroy;
begin
  FMailContent.Free;
  FAttachFiles.Free;
  FAttachFilesType.Free;
  FAttachFilesID.Free;
  SMTPComponent.Free;
  EMailComponent.Free;
  LHandlerComponent.Free;
  inherited ;
end;

procedure ThbMailSender.Disconnect;
begin
 LHandlerComponent.CloseGracefully;
 SMTPComponent.Socket.Close;
 SMTPComponent.Disconnect;
end;

Procedure ThbMailSender.WLines(Value: TStringList);
begin
  FMailContent.Assign(Value);
end;


Procedure ThbMailSender.WAttachFiles(Value: TStringList);
begin
  FAttachFiles.Assign(Value);
end;

Procedure ThbMailSender.WAttachFilesType(Value: TStringList);
begin
  FAttachFilesType.Assign(Value);
end;

Procedure ThbMailSender.WAttachFilesID(Value: TStringList);
begin
  FAttachFilesID.Assign(Value);
end;


Procedure ThbMailSender.Connect;
begin
  if not SMTPComponent.Connected then
  begin
   SMTPComponent.Host := FSMTPHost;
   SMTPComponent.AuthType := satDefault;
   SMTPComponent.Username := FSMTPMailAddress;
   SMTPComponent.Password := FSMTPMailPassword;
   SMTPComponent.Port := FSMTPPort;

   LHandlerComponent.Destination := FSMTPHost + ':' + IntToStr(FSMTPPort);
   LHandlerComponent.Host := FSMTPHost;
   LHandlerComponent.Port := FSMTPPort;
   LHandlerComponent.DefaultPort := 0;
   LHandlerComponent.SSLOptions.Method := sslvTLSv1;
   LHandlerComponent.SSLOptions.Mode := sslmUnassigned;
   LHandlerComponent.SSLOptions.VerifyMode := [];
   LHandlerComponent.SSLOptions.VerifyDepth := 0;

   SMTPComponent.IOHandler := LHandlerComponent;
   SMTPComponent.UseTLS := utUseExplicitTLS;
   SMTPComponent.ConnectTimeout := FConnectTimeOut;

   SMTPComponent.Connect;
  end;
 end;



procedure ThbMailSender.SendMail;
var
 I: Integer;
 Rep: string;
begin
 try
   EMailComponent.Clear;
   EMailComponent.From.Address := FSMTPMailAddress;
   EMailComponent.From.Name := FSMTPName;
   EMailComponent.ReplyTo.EMailAddresses := FMailReplyToAddress;
   EMailComponent.Recipients.Add.Name :=  FClientMailName;
   EMailComponent.Recipients.EMailAddresses := FClientMailAddress;
   EMailComponent.Subject := FMailSubject;
   EMailComponent.ContentType := FContentType;
   EMailComponent.CharSet := 'utf-8';

   if Trim(FAttachFiles.Text) <> '' then
   begin
     if FAttachFiles.Count <> 0 then
     begin
       FAttachFileName := '';

       if (FAttachFiles.Count <> FAttachFilesType.Count) or (FAttachFiles.Count <> FAttachFilesID.Count) then
       begin
         raise Exception.Create(PChar('There'+#39+'s a problem with multiple files you'+#39+'re about to send!' + sLineBreak + sLineBreak +'Tip: AttachFiles, AttachFilesType, AttachFilesID quantities must be the same. You will be directed to the "help" link for a better description.'));
         exit;
       end
       else
       begin
         for I := 0 to FAttachFiles.Count - 1 do
         begin
           if FileExists(FAttachFiles.Strings[I], True) = True then
           begin
             with TIdAttachmentFile.Create(EMailComponent.MessageParts, FAttachFiles.Strings[I]) do
             begin
               ContentType := FAttachFilesType.Strings[I];
               ContentID := FAttachFilesID.Strings[I];
               DisplayName := ExtractFileName(FAttachFiles.Strings[I]);
               FileName := ExtractFileName(FAttachFiles.Strings[I]);
             end;
           end;
         end;
       end;
     end;
   end;

    if FAttachFileName.Length <> 0 then
   begin
     if FileExists(FAttachFileName, True) = True then
     begin
       with TIdAttachmentFile.Create(EMailComponent.MessageParts, FAttachFileName) do
      begin
       ContentType := FAttachType;
       ContentID := FContentID;
       DisplayName := ExtractFileName(FAttachFileName);
       FileName := ExtractFileName(FAttachFileName);
      end;
     end;
   end;

   EMailComponent.Body.Text := FMailContent.Text;

   SMTPComponent.Send(EMailComponent);
 finally
   EMailComponent.Clear;
 end;
end;
end.
