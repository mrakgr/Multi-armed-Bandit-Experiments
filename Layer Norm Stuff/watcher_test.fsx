// The filesystem watcher is giving me a lot of trouble, so let me try and see if I can make it trigger using the MSDN example.

open System
open System.IO

// Create a new FileSystemWatcher and set its properties.
let watcher = new FileSystemWatcher();
watcher.Path <- @"C:\Temp"
watcher.NotifyFilter <- NotifyFilters.LastAccess ||| NotifyFilters.LastWrite
    ||| NotifyFilters.FileName ||| NotifyFilters.DirectoryName;
// Only watch text files.
watcher.Filter <- "*.txt";

let OnChanged (e: FileSystemEventArgs) = Console.WriteLine("File: " +  e.FullPath + " " + string e.ChangeType);
let OnRenamed (e: RenamedEventArgs) = Console.WriteLine("File: {0} renamed to {1}", e.OldFullPath, e.FullPath);

// Add event handlers.
watcher.Changed.Add <| OnChanged
watcher.Created.Add <| OnChanged
watcher.Deleted.Add <| OnChanged
watcher.Renamed.Add <| OnRenamed

// Begin watching.
watcher.EnableRaisingEvents <- true

// Wait for the user to quit the program.
Console.WriteLine("Press \'q\' to quit the sample.");
while (Console.Read() <> int 'q') do ()
