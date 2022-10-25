# MakeP4DComponents

Provides a user-friendly 'Wizard' to allow the creation of Delphi Components for the P4D Suite of projects

<u>This is an alpha version so expect bugs</u>

A full tutorial will be forthcoming but for now here's a crash course

To install just download the Release, extract it and double click on MakeP4DComponents.

The window you get looks like this...

![mainForm](D:\src\MakeP4DComponents\images\mainForm.png)

This form allows you to enter the holistic overview of the collection of components you are creating. The fields in the form are mainly for some housekeeping that Delphi wants to know but there are others that automatically create a README.md for you such as the text entry area labelled README.md Text.
Directly under this box is a CheckBox that, if ticked, will add the details from the Python packages (getting to that part next) you include. The addition of the optional fields is not mandatory but it makes sense to enter as much information as possible.  

A really important field on the above page is the Palette Page. This is where all the components you create will be bundled together in the Delphi Tool Palette.

Click on the Add Package button an a new window will open allowing you to enter the details of a Python Package.

![componentForm](D:\src\MakeP4DComponents\images\componentForm.png)

The first five fields are mandatory as these define the individual component you are adding. The Default Icon (by Jim McKeeth) is there so that you at least have some image. If you have the time obtain a proper icon and change the one provided by clicking on the Default icon and loading in your preferred image.

The lower half of the form is for handy web addresses. These are guessed at in the case of PyPi and ReadTheDocs. The Delphi Class Name at the top will trigger a quick look at both sites to see if there is anything there - if so those will be filled in for you (I might get them wrong, yu can check by clicking the Open button which will open the URL you have enabling you to see if I guessed right). The PyPi page sometimes has a Homepage, especially if part of a group of packages, and quite regularly you can get a Github page to fill in on the form as well.

The advantage of filling all this information in becomes clear when you export the component as it creates a README.md with all that information on it all ready to upload to a Git server.

### The Menu

The menu allows you to Load, Save and Reset the form. The REALLY important options are, however, the Export ones. You can either output direclty to disk - this will build a directory structure with YourProject.groupproj in the root - double click on that and Delphi will open and you can install your package ([see my tutorial](https://peardox.com/mixing-python-and-delphi-made-easy/) for full details on installation) 

The other option is to output the component package as a Zip file with the same files as exporting to disk gets you (this was one of my favourite bits of writing this - creating a ZIP on the fly without any temp files...)

### It Should Be Noted

Every time you open MakeP4DComponents in the future it'll be exactly as last time you closed it. This is by design. I HATE forgetting to save my work so this won't let me. If you want a clean slate then just File -> Reset and you'll be at the starting point again. The Save option allow does what you'd expect as does the Load one :) 

Right click on an icon on the Main page if you want the option to delete a package. Left click to edit it.

There is a Help item in the menu - I've not got that far yet...

There's also an Offline mode - this will just stop it trying to guess Website addresses - which slows things down marginally and you may be away from WiFi or whatever.

MakeP4DComponents saves it's data as p4d files - they're just JSON. If you and someone else both have MakeP4DComponents you can just send them the p4d and they'll get a copy of your project.

This also allows some flexibility - I might already have a component installed I make with MakeP4DComponents included in a p4d that you sent me. I can simply load your project, delete your version and export a new package without the duplication

### What it looks like in action

There is a sample p4d included in the repo at https://github.com/peardox/TestComponentPackage that resulted from my second test run (the first had *really bad* MarkDown rather than just poor MarkDown) You can load it into MakeP4DComponents and you'll get this test set of components (I need to do proper Icons for) that will be used in a future Tutorial.

![TestRun](D:\src\MakeP4DComponents\images\TestRun.png)

Supporting the following projects (with special note to the last two)

https://github.com/Embarcadero/python4delphi

https://github.com/Embarcadero/PythonEnviroments

https://github.com/Embarcadero/Lightweight-Python-Wrappers

https://github.com/Embarcadero/P4D-Data-Sciences

https://github.com/Embarcadero/PythonPackages4Delphi







Make sure you "brcc32 EmbeddedResources.rc" before trying to build if you alter any resources

