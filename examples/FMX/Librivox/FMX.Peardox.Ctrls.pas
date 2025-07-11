unit FMX.Peardox.Ctrls;

interface

{$SCOPEDENUMS ON}

uses
  System.Classes, System.Types, System.Rtti, System.UITypes, System.Messaging, System.Math, System.ImageList,
  FMX.ActnList, FMX.Types, FMX.Objects, FMX.Ani, FMX.StdActns, FMX.Controls, FMX.Graphics, FMX.Controls.Presentation,
  FMX.Controls.Model, FMX.ImgList, FMX.AcceleratorKey;

type

{ TProgressBar }

  TProgressBar = class(TPresentedControl, IValueRange)
  private
    FOrientation: TOrientation;
    FValueRange: TValueRange;
    FDefaultValueRange: TBaseValueRange;
    procedure SetOrientation(const Value: TOrientation);
    function GetMax: Double;
    function GetMin: Double;
    function GetValue: Double;
    procedure SetMax(const Value: Double);
    procedure SetMin(const Value: Double);
    procedure SetValue(const Value: Double);
    function GetValueRange: TCustomValueRange;
    procedure SetValueRange(const AValue: TCustomValueRange);
    function DefStored: Boolean;
    procedure ChangedProc(Sender: TObject);
    function MaxStored: Boolean;
    function MinStored: Boolean;
    function ValueStored: Boolean;
  protected
    function ChooseAdjustType(const FixedSize: TSize): TAdjustType; override;
    procedure ApplyStyle; override;
    procedure DefineProperties(Filer: TFiler); override;
    function GetData: TValue; override;
    procedure SetData(const Value: TValue); override;
    procedure DoRealign; override;
    function GetActionLinkClass: TActionLinkClass; override;
    procedure ActionChange(Sender: TBasicAction; CheckDefaults: Boolean); override;
    procedure AfterChangeProc(Sender: TObject); virtual;
    property DefaultValueRange: TBaseValueRange read FDefaultValueRange;
    procedure Loaded; override;
    function GetDefaultSize: TSizeF; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure AfterConstruction; override;
    destructor Destroy; override;
  published
    property Action;
    property Align;
    property Anchors;
    property ClipChildren default False;
    property ClipParent default False;
    property ControlType;
    property Cursor default crDefault;
    property DragMode default TDragMode.dmManual;
    property EnableDragHighlight default True;
    property Enabled;
    property Locked default False;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property HitTest default True;
    property Padding;
    property Max: Double read GetMax write SetMax stored MaxStored nodefault;
    property Min: Double read GetMin write SetMin stored MinStored nodefault;
    property Opacity;
    property Orientation: TOrientation read FOrientation write SetOrientation;
    property Margins;
    property PopupMenu;
    property Position;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property Size;
    property StyleLookup;
    property TouchTargetExpansion;
    property Value: Double read GetValue write SetValue stored ValueStored nodefault;
    property Visible;
    property Width;
    property ParentShowHint;
    property ShowHint;
    {events}
    property OnApplyStyleLookup;
    property OnFreeStyle;
    {Drag and Drop events}
    property OnDragEnter;
    property OnDragLeave;
    property OnDragOver;
    property OnDragDrop;
    property OnDragEnd;
    {Keyboard events}
    property OnKeyDown;
    property OnKeyUp;
    {Mouse events}
    property OnCanFocus;
    property OnClick;
    property OnDblClick;

    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseEnter;
    property OnMouseLeave;

    property OnPainting;
    property OnPaint;
    property OnResize;
    property OnResized;
  end;

implementation

uses
  System.UIConsts, System.Actions, System.RTLConsts, System.TypInfo, System.SysUtils, System.Math.Vectors,
  System.Character, FMX.Consts, FMX.Utils, FMX.Platform, FMX.Dialogs, FMX.TextLayout, FMX.BehaviorManager, FMX.Forms,
  FMX.Switch.Style, FMX.Styles, FMX.Styles.Objects, FMX.Platform.Metrics {$IFDEF IOS}, FMX.Switch.iOS{$ENDIF}
  {$IFDEF MSWINDOWS}, FMX.Switch.Win{$ENDIF}{$IFDEF ANDROID}, FMX.Switch.Android{$ENDIF};

{ TProgressBar }

constructor TProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  FValueRange := TValueRange.Create(Self);
  FDefaultValueRange := TBaseValueRange.Create;
  FValueRange.AfterChange := AfterChangeProc;
  FValueRange.OnChanged := ChangedProc;
  CanFocus := False;
  SetAcceptsControls(False);
end;

procedure TProgressBar.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TabOrder', IgnoreIntegerValue, nil, False);
end;

destructor TProgressBar.Destroy;
begin
  FreeAndNil(FDefaultValueRange);
  FreeAndNil(FValueRange);
  inherited;
end;

procedure TProgressBar.AfterConstruction;
begin
  inherited;
  DefaultValueRange.Assign(FValueRange.New);
end;

procedure TProgressBar.Loaded;
begin
  if FValueRange.IsChanged then
    FValueRange.Changed(True);
  inherited;
end;

function TProgressBar.GetData: TValue;
begin
  Result := Value;
end;

function TProgressBar.GetDefaultSize: TSizeF;
begin
  Result := TSizeF.Create(100, 20);
end;

procedure TProgressBar.SetData(const Value: TValue);
begin
  if Value.IsOrdinal then
    Self.Value := Value.AsOrdinal
  else
  if Value.IsType<Single> then
    Self.Value := Value.AsType<Single>
  else
    Self.Value := Min
end;

function TProgressBar.GetActionLinkClass: TActionLinkClass;
begin
  Result := TValueRangeActionLink;
end;

procedure TProgressBar.ActionChange(Sender: TBasicAction; CheckDefaults: Boolean);
begin
  if Sender is TValueRangeAction then
  begin
    if (not CheckDefaults) or (FValueRange.IsEmpty) then
      FValueRange.Assign(TValueRangeAction(Sender).ValueRange);
  end;
  inherited;
end;

function TProgressBar.GetValueRange: TCustomValueRange;
begin
  Result := FValueRange;
end;

function TProgressBar.MaxStored: Boolean;
begin
  Result := DefStored and (not SameValue(Max, DefaultValueRange.Max));
end;

function TProgressBar.MinStored: Boolean;
begin
  Result := DefStored and (not SameValue(Min, DefaultValueRange.Min));
end;

function TProgressBar.ValueStored: Boolean;
begin
  Result := DefStored and (not SameValue(Value, DefaultValueRange.Value));
end;

procedure TProgressBar.SetValueRange(const AValue: TCustomValueRange);
begin
  FValueRange.Assign(AValue);
end;

function TProgressBar.GetMax: Double;
begin
  Result := FValueRange.Max - FValueRange.ViewportSize;
end;

procedure TProgressBar.SetMax(const Value: Double);
var
  V: Double;
begin
  V := Value + FValueRange.ViewportSize;
  if FValueRange.Max <> V then
    FValueRange.Max := V;
end;

function TProgressBar.GetMin: Double;
begin
  Result :=  FValueRange.Min;
end;

procedure TProgressBar.SetMin(const Value: Double);
begin
  if FValueRange.Min <> Value then
    FValueRange.Min := Value;
end;

function TProgressBar.GetValue: Double;
begin
  Result := FValueRange.Value;
end;

procedure TProgressBar.SetValue(const Value: Double);
begin
  if FValueRange.Value <> Value then
    FValueRange.Value := Value;
end;

procedure TProgressBar.ApplyStyle;
var
  Indicator: TFmxObject;
  IndicatorStyleName: string;
begin
  inherited;
  if Orientation = TOrientation.Horizontal then
    IndicatorStyleName := 'hindicator'
  else
    IndicatorStyleName := 'vindicator';

  if FindStyleResource<TFmxObject>(IndicatorStyleName, Indicator) then
    TAnimator.StartTriggerAnimation(Indicator, Self, 'IsVisible');
  Realign;
end;

procedure TProgressBar.AfterChangeProc(Sender: TObject);
begin
  if ActionClient and (not DefStored) and (not TValueRangeAction(Action).ValueRange.Changing) then
    TValueRangeAction(Action).ValueRange := FValueRange;
end;

procedure TProgressBar.ChangedProc(Sender: TObject);
begin
  Realign;
end;

function TProgressBar.ChooseAdjustType(const FixedSize: TSize): TAdjustType;
begin
  if FixedSize.Height <> 0 then
  begin
    if Orientation = TOrientation.Horizontal then
      Result := TAdjustType.FixedHeight
    else
    begin
      SetAdjustSizeValue(TSizeF.Create(AdjustSizeValue.Height, AdjustSizeValue.Width));
      Result := TAdjustType.FixedWidth;
    end;
  end
  else
    Result := TAdjustType.None;
end;

function TProgressBar.DefStored: Boolean;
begin
  Result := not (ActionClient and (Action is TCustomValueRangeAction));
end;

procedure TProgressBar.DoRealign;
var
  HorizontalIndicator, VerticalIndicator, T: TControl;
  NewSize: Single;
begin
  inherited;
  if not FDisableAlign then
  begin
    FDisableAlign := True;
    try
      T := nil;
      case Orientation of
        TOrientation.Horizontal:
        begin
          if FindStyleResource<TControl>('vtrack', T) then
            T.Visible := False;
          FindStyleResource<TControl>('htrack', T);
        end;
        TOrientation.Vertical:
        begin
          if FindStyleResource('htrack', T) then
            T.Visible := False;
          FindStyleResource<TControl>('vtrack', T);
        end;
      end;
      if T = nil then
        FindStyleResource<TControl>('track', T);
      if T <> nil then
      begin
        T.Visible := True;
        if Max > Min then
        begin
          HorizontalIndicator := nil;
          FindStyleResource<TControl>('hindicator', HorizontalIndicator);
          VerticalIndicator := nil;
          FindStyleResource<TControl>('vindicator', VerticalIndicator);
          if Orientation = TOrientation.Horizontal then
          begin
            if HorizontalIndicator <> nil then
            begin
              NewSize := (T.Width - T.Padding.Left - T.Padding.Right - HorizontalIndicator.Margins.Left - HorizontalIndicator.Margins.Right)
                * FValueRange.RelativeValue;
              HorizontalIndicator.Width := Round(NewSize);
              HorizontalIndicator.Visible := HorizontalIndicator.Width > 2;
            end;
            if VerticalIndicator <> nil then
              VerticalIndicator.Visible := False;
          end
          else
          begin
            if VerticalIndicator <> nil then
            begin
              NewSize := (T.Height - T.Padding.Top - T.Padding.Bottom - HorizontalIndicator.Margins.Top - HorizontalIndicator.Margins.Bottom)
                * FValueRange.RelativeValue;
              VerticalIndicator.Height := Round(NewSize);
              VerticalIndicator.Visible := VerticalIndicator.Height > 2;
            end;
            if HorizontalIndicator <> nil then
              HorizontalIndicator.Visible := False;
          end;
        end;
      end;
    finally
      FDisableAlign := False;
    end;
  end;
end;

procedure TProgressBar.SetOrientation(const Value: TOrientation);
var
  SavedSize: TSizeF;
begin
  if FOrientation <> Value then
  begin
    SavedSize := Size.Size;

    FOrientation := Value;
    NeedStyleLookup;
    ApplyStyleLookup;

    if not (csLoading in ComponentState) then
      SetBounds(Position.X, Position.Y, SavedSize.Height, SavedSize.Width);
  end;
end;
