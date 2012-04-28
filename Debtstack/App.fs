// Copyright 2012 Max Battcher. Some rights reserved.
// Licensed for use under the Ms-RL. See attached LICENSE file.
namespace Debtstack

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Markup

module MainApp =
    // Application Entry point
    [<STAThread>]
    [<EntryPoint>]
    let main(_) =
    // Create the View and bind it to the View Model
        let mainWindow = Application.LoadComponent(
                             new System.Uri("/Debtstack;component/mainwindow.xaml", UriKind.Relative)) :?> Window
        (new Application()).Run(mainWindow)