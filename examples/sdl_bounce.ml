let () =
    Sdl.init [`VIDEO];
    Sdlvideo.set_video_mode 800 800 [];
    Sdltimer.delay 2000;
    Sdl.quit ()
