object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 346
  ClientWidth = 513
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object WebBrowser1: TWebBrowser
    Left = 21
    Top = 27
    Width = 463
    Height = 253
    TabOrder = 0
    ControlData = {
      4C000000DA2F0000261A00000000000000000000000000000000000000000000
      000000004C000000000000000000000001000000E0D057007335CF11AE690800
      2B2E126208000000000000004C0000000114020000000000C000000000000046
      8000000000000000000000000000000000000000000000000000000000000000
      00000000000000000100000000000000000000000000000000000000}
  end
  object Button1: TButton
    Left = 42
    Top = 297
    Width = 106
    Height = 25
    Caption = 'Call HelloJS Func'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 186
    Top = 297
    Width = 109
    Height = 25
    Caption = 'Get HTML Source'
    TabOrder = 2
    OnClick = Button2Click
  end
  object ApplicationEvents1: TApplicationEvents
    OnMessage = ApplicationEvents1Message
    Left = 354
    Top = 294
  end
end
