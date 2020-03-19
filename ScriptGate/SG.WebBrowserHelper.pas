unit SG.WebBrowserHelper;

interface

uses
  Vcl.OleCtrls, SHDocVw, Controls, SysUtils, Classes, ActiveX;

type
  TWebBrowserHelper = class helper for TWebBrowser
  public
    procedure SetForHybridApp;
    procedure SetUserAgent(const iUA: String);
    function CheckBack(const iKey: Word): Boolean;
    procedure Hide;
    procedure LoadFromStrings(const Content: string; const BaseUrl: string);
    procedure Show;
  end;

implementation

uses
  System.UITypes, System.Rtti, System.Generics.Collections, Types, Forms;
// , VirtualKeyboard;

type

  TBrowserProps = record
    Align: TAlign;
    X: Single;
  end;

  TBrowserPropsDic = TDictionary<TWebBrowser, TBrowserProps>;

var
  GBrowserProps: TBrowserPropsDic;

  { TWebBrowserHelper }

function TWebBrowserHelper.CheckBack(const iKey: Word): Boolean;
begin
  Result := False;
  if (iKey = vkHardwareBack) then
  begin
    // if (Self.CanGoBack) then
    // begin
    // Self.GoBack;
    // Result := True;
    // end;
  end
end;

procedure TWebBrowserHelper.Hide;
var
  Props: TBrowserProps;
begin
  if (Self.Parent = nil) then
    Exit;

  Props.Align := Self.Align;
  Props.X := Self.Left; // self.Position.X;

  GBrowserProps.AddOrSetValue(Self, Props);

  Self.Align := TAlign.alNone;
  // Self.SetBounds(Screen.Width, Position.Y, Width, Height);
  Self.SetBounds(Screen.Width, Top, Width, Height);
end;

procedure TWebBrowserHelper.LoadFromStrings(const Content, BaseUrl: string);

  procedure LoadDocumentFromStream(const Stream: TStream);
  const
    CBlankPage: string = 'about:blank'; // do not localize
  var
    PersistStreamInit: IPersistStreamInit;
    StreamAdapter: IStream;
  begin
    if Self.Document = nil then
      Self.Navigate(CBlankPage);
    if Self.Document.QueryInterface(IPersistStreamInit, PersistStreamInit) = S_OK
    then
    begin
      if PersistStreamInit.InitNew = S_OK then
      begin
        StreamAdapter := TStreamAdapter.Create(Stream);
        PersistStreamInit.Load(StreamAdapter);
      end;
    end;
  end;

var
  StringStream: TStringStream;
begin
  StringStream := TStringStream.Create(Content);
  try
    LoadDocumentFromStream(StringStream);
  finally
    StringStream.Free;
  end;
end;

procedure TWebBrowserHelper.SetForHybridApp;
begin
end;

procedure TWebBrowserHelper.SetUserAgent(const iUA: String);
begin
end;

procedure TWebBrowserHelper.Show;
var
  Props: TBrowserProps;
begin
  if (Self.Parent = nil) then
    Exit;

  if (GBrowserProps.TryGetValue(Self, Props)) then
  begin
    // Self.SetBounds(Props.X, Position.Y, Width, Height);
    Self.SetBounds(Left, Top, Width, Height);
    Self.Align := Props.Align;
  end;
end;

initialization

GBrowserProps := TBrowserPropsDic.Create;

finalization

FreeAndNil(GBrowserProps);

end.
