
{$IF DEFINED(FPC)}
  {$IF DEFINED(LINUX64) and DEFINED(CPUX86_64)}
    {$DEFINE OS_LINUX64}
  {$ELSEIF DEFINED(LINUX64) and DEFINED(CPUAARCH64)}
    {$DEFINE OS_LINUX64ARM}
  {$ELSEIF DEFINED(MSWINDOWS) and DEFINED(CPUX86_64)}
    {$DEFINE OS_WIN64}
  {$ELSEIF DEFINED(MSWINDOWS) and DEFINED(CPUAARCH64)}
    {$DEFINE OS_WIN64ARM}
  {$ELSEIF DEFINED(DARWIN) and DEFINED(CPUAARCH64)}
    {$DEFINE OS_OSXARM64}
  {$ELSEIF DEFINED(DARWIN) and DEFINED(CPUX86_64)}
    {$DEFINE OS_OSX64}
  {$ELSE}
    Unsupported Platform
  {$ENDIF}
{$ELSE}
  {$IF DEFINED(LINUX64) and DEFINED(CPUX64)}
    {$DEFINE OS_LINUX64)}
  {$ELSEIF DEFINED(MSWINDOWS) and DEFINED(CPUX64)}
    {$DEFINE OS_WIN64)}
  {$ELSEIF DEFINED(OSX64) and DEFINED(CPUARM64)}
    {$DEFINE OS_OSXARM64)}
  {$ELSEIF DEFINED(OSX64) and DEFINED(CPUX64)}
    {$DEFINE OS_OSX64)}
  {$ELSEIF DEFINED(ANDROID64)}
    {$DEFINE OS_ANDROID64)}
  {$ELSEIF DEFINED(IOS)}
    {$DEFINE OS_IOS)}
  {$ELSE}
    Unsupported Platform
  {$ENDIF}
{$ENDIF}
