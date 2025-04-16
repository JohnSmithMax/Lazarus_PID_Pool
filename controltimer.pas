unit ControlTimer;

{$mode objfpc}{$H+}

interface

uses
  Classes, ExtCtrls, Definitions;

type
  IControlTimer = interface
    ['{8D5F6C03-4D5A-4F1B-9C3A-7E9D3B2F4E1D}'] // GUID для интерфейса
    procedure Start;
    procedure Stop;
    function GetOnTick: TNotifyEvent;
    procedure SetOnTick(Value: TNotifyEvent);
    property OnTick: TNotifyEvent read GetOnTick write SetOnTick;
  end;

  TControlTimer = class(TInterfacedObject, IControlTimer)
  private
    FTimer: TTimer;
    FOnTick: TNotifyEvent;
    procedure TimerTick(Sender: TObject);
    function GetOnTick: TNotifyEvent;
    procedure SetOnTick(Value: TNotifyEvent);
  public
    constructor Create(Interval: Integer);
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property OnTick: TNotifyEvent read GetOnTick write SetOnTick;
  end;

implementation

constructor TControlTimer.Create(Interval: Integer);
begin
  FTimer := TTimer.Create(nil);
  FTimer.Interval := PID_UPDATE_INTERVAL_MS;
  FTimer.OnTimer := @TimerTick;
end;

destructor TControlTimer.Destroy;
begin
  FTimer.Free;
  inherited Destroy;
end;

procedure TControlTimer.TimerTick(Sender: TObject);
begin
  if Assigned(FOnTick) then
    FOnTick(Self);
end;

procedure TControlTimer.Start;
begin
  FTimer.Enabled := True;
end;

procedure TControlTimer.Stop;
begin
  FTimer.Enabled := False;
end;

function TControlTimer.GetOnTick: TNotifyEvent;
begin
  Result := FOnTick;
end;

procedure TControlTimer.SetOnTick(Value: TNotifyEvent);
begin
  FOnTick := Value;
end;

end.
