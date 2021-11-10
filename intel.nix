{ ... }: {
  #boot.kernelModules = ["i915" "kvm-intel"];
  #services.xserver.videoDrivers = ["intel"];

  hardware.opengl.enable = true;
  hardware.opengl.driSupport = true;

  hardware.cpu.intel.updateMicrocode = true;
}
