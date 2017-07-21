unit NichtDoppeltStarten;

interface

implementation

uses Windows;

var mHandle: THandle;   // Mutex handle

Initialization
  mHandle := CreateMutex(nil, true, 'HeliumManagement');
  if GetLastError = ERROR_ALREADY_EXISTS then Halt;

Finalization
  if (mHandle <> 0) then CloseHandle(mHandle)
end.
