unit Definitions;

{$mode objfpc}{$H+}

interface

uses
  SysUtils;


const
  // Настройки окна
  WINDOW_WIDTH = 900;
  WINDOW_HEIGHT = 600;
  WINDOW_TITLE = 'HMI Управление бассейном с ПИД-регулятором';

  BUTTON_START_CAPTION = 'Старт';
  BUTTON_STOP_CAPTION = 'Стоп';
  BUTTON_RESET_CAPTION = 'Сброс';

  // Параметры ПИД-регулятора
  KP = 0.7;
  KI = 0.1;
  KD = 0.2;
  MAX_PID_OUTPUT = 10.0;
  MIN_PID_OUTPUT = 0.0;
  PID_ENABLED_DEFAULT = False;
  RESET_INTEGRAL_TO_START = 0;

  // Уровни воды
  START_WATER_LEVEL = 50;
  TARGET_POOL_LEVEL = 70;
  MAX_POOL_LEVEL = 100;
  POOL_DEFAULT_LEVEL = 50;
  OUTFLOW_RATE = 5;

  // Настройки слайдеров
  MIN_SLIDER = 0;
  MAX_INFLOW_SLIDER = 10;
  MAX_OUTFLOW_SLIDER = 9;

  // Таймеры
  UPDATE_TIMER_MS = 100;
  WATER_LEVEL_UPDATE_RATIO = UPDATE_TIMER_MS / 1000;
  PID_UPDATE_INTERVAL_MS = 200;

  // Текстовые метки
  INFLOW_PIPE = 'Труба наполнения:';
  OUTFLOW_PIPE = 'Труба слива:';

  // Настройки графика
  GRAPH_MINIMUM_HEIGHT = 150;
  GRAPH_MAX_POINTS = 200;
  GRAPH_MARGIN_TOP = 20;
  GRAPH_MARGIN_BOTTOM = 10;
  GRAPH_LINE_WIDTH = 2;
  GRAPH_BG_COLOR = $F0F0F0;
  WATER_LINE_COLOR = $C80000;
  TARGET_LINE_COLOR = $0000FF;


  // Настройки бассейна
  POOL_BG_COLOR = $C8FFC8;
  POOL_BG_LINE_COLOR = $006400;
  POOL_WATER_LINE_COLOR = $640000;
  POOL_WATER_COLOR = $FF6464;
  TARGET_LEVEL_COLOR = $0000FF;


  function PID_CHECKBOX_LABEL: string;

implementation

function PID_CHECKBOX_LABEL: string;
begin
  Result := Format('Включить ПИД-регулятор (цель: %d%%)', [TARGET_POOL_LEVEL]);
end;


end.
