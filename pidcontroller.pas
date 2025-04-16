unit PIDController;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DateUtils;

type
  TPIDController = class
  private
    FKp: Double;
    FKi: Double;
    FKd: Double;
    FSetpoint: Double;
    FLastError: Double;
    FIntegral: Double;
    FLastTime: TDateTime;
  public
    constructor Create(Kp, Ki, Kd, Setpoint: Double);
    function Compute(CurrentValue: Double): Double;
  end;

implementation

uses
  Definitions;

constructor TPIDController.Create(Kp, Ki, Kd, Setpoint: Double);
begin
  FKp := Kp;
  FKi := Ki;
  FKd := Kd;
  FSetpoint := Setpoint;
  FLastError := 0;
  FIntegral := 0;
  FLastTime := Now;
end;

function TPIDController.Compute(CurrentValue: Double): Double;
var
  NowTime: TDateTime;
  dt: Double;
  error: Double;
  P, I, D: Double;
  derivative: Double;
  output: Double;
begin
  NowTime := Now;
  dt := MilliSecondsBetween(NowTime, FLastTime) / 1000;
  if dt <= 0 then
    dt := 0.01;

  error := FSetpoint - CurrentValue;

  // Пропорциональная составляющая
  P := FKp * error;

  // Интегральная составляющая
  FIntegral := FIntegral + error * dt;
  I := FKi * FIntegral;

  // Дифференциальная составляющая
  derivative := (error - FLastError) / dt;
  D := FKd * derivative;

  FLastError := error;
  FLastTime := NowTime;

  output := P + I + D;

  // Защита от разгона интегральной компоненты
  if output > MAX_PID_OUTPUT then
    FIntegral := FIntegral / 2;

  // Ограничение выхода
  if output < MIN_SLIDER then
    output := MIN_SLIDER
  else if output > MAX_INFLOW_SLIDER then
    output := MAX_INFLOW_SLIDER;

  Result := output;
end;

end.
