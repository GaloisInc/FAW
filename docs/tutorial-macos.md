
Valid as of 2023-08-17

Following are incomplete notes for getting the PDF distribution of the FAW to run on Mac OS X systems running the ARM architecture:

* Bump the Haskell version (8.8 does not have an arm64-friendly image on Docker Hub - 8.10 worked for me, but apparently, per Hari, 9.4.5 would work as well)
* Comment out the line '/usr/lib/x86_64-linux-gnu/valgrind': true, in pdf/plugin-pdf-valgrind/config.json5 (as you might expect, that file does not exist on non-x86 platforms, and I don't know where its arm equivalent would be)
* Exclude the tika parser from being built altogether (the image, maven:3.6.3-ibmjava-8-alpine, has no arm64 support, and I don't know enough about tika or maven or java to intelligently guess how to bump the version without breaking something)
* Exclude the ml_test parser from being built altogether (pip install -r requirements.txt fails with Could not find a version that satisfies the requirement torch==1.8.0+cpu (from versions: 1.7.1, 1.8.0, 1.8.1, 1.9.0, 1.10.0, 1.10.1, 1.10.2, 1.11.0, 1.12.0, 1.12.1, 1.13.0, 1.13.1, 2.0.0, 2.0.1))
* Give Docker enough memory (16G worked for me)
