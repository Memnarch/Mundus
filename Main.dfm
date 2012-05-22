object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'SoftwareRenderer'
  ClientHeight = 537
  ClientWidth = 512
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object RenderInfo: TLabel
    Left = 8
    Top = 512
    Width = 174
    Height = 19
    Caption = 'Some Render Infos Here'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object GameScreen: TPaintBox
    Left = 0
    Top = 0
    Width = 512
    Height = 512
  end
  object lbInfo: TLabel
    Left = 312
    Top = 512
    Width = 174
    Height = 19
    Caption = 'Some Render Infos Here'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object GameTimer: TTimer
    Enabled = False
    Interval = 1
    OnTimer = GameTimerTimer
    Left = 256
    Top = 488
  end
end
