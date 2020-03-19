unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.OleCtrls, SHDocVw, SG.ScriptGate,
  SG.WebBrowserHelper, Vcl.StdCtrls, Vcl.AppEvnts;

type
  TForm1 = class(TForm)
    WebBrowser1: TWebBrowser;
    Button1: TButton;
    Button2: TButton;
    ApplicationEvents1: TApplicationEvents;
    procedure ApplicationEvents1Message(var Msg: tagMSG; var Handled: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
  private
    FScriptGate: TScriptGate;
  public
    procedure Add(const Msg: String; const A, B: Integer);
    procedure HelloDelphi(const iStr: String);
  end;

var
  Form1: TForm1;

const
  SampleHTML = '<html>' + '<header>' + '<script type="text/JavaScript">' +
    'function helloJS(msg, msg2) { alert(msg + msg2); return "Hello Delphi ! I am JavaScript !"; }'
    +
  // Call from Delphi
    '</script>' + '</head>' + '<body>' + '<br><br>' + // Call Delphi Method
    '<a href="YourOrgScheme:HelloDelphi(''call by JS'')">Call Delphi noparam procedure</a>'
    + '<br><br>' +
    '<a href="YourOrgScheme:Add(''Calc: 30 + 13 = '', 30, 13)">Call Delphi procedure</a>'
    + '<body>' + '</html>';

implementation

{$R *.dfm}

procedure TForm1.Add(const Msg: String; const A, B: Integer);
begin
  ShowMessage(Msg + (A + B).ToString);
end;

procedure TForm1.ApplicationEvents1Message(var Msg: tagMSG;
  var Handled: Boolean);
begin
  // 替换右键菜单
  // var
  // mPoint: TPoint;
  // begin
  // if IsChild(WebBrowser.Handle, Msg.Hwnd) and
  // ((Msg.Message = WM_RBUTTONDOWN) or (Msg.Message = WM_RBUTTONUP)) then
  // begin
  // GetCursorPos(mPoint); //得到光标位置
  // pm5.Popup(mPoint.X, mPoint.Y); //弹出popupmenu1的菜单
  // Handled := True;
  // end;
  // 屏蔽右键菜单
  with Msg do
  begin
    if not IsChild(WebBrowser1.Handle, hWnd) Then
      Exit;
    Handled := (message = WM_RBUTTONDOWN) or (message = WM_RBUTTONUP) or
      (message = WM_CONTEXTMENU);
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  FScriptGate.CallScript('helloJS("Hello JS ! ", "I am Delphi !")',
    procedure(const iResult: String)
    begin
      ShowMessage(iResult);
    end);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  FScriptGate.Eval('document.getElementsByTagName("html")[0].outerHTML',
    procedure(const iResult: String)
    begin
      ShowMessage(iResult);
    end);
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FScriptGate.Free;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  WebBrowser1.LoadFromStrings(SampleHTML, '/');
  // The method of the object specified by the first argument is
  // called from JavaScript.
  FScriptGate := TScriptGate.Create(Self, WebBrowser1, 'YourOrgScheme');
end;

procedure TForm1.HelloDelphi(const iStr: String);
begin
  ShowMessage('Delphi: ' + iStr);
end;

end.
