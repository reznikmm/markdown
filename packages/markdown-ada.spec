# SPDX-FileCopyrightText: 2020 Max Reznik <reznikmm@gmail.com>
#
# SPDX-License-Identifier: MIT

%undefine _hardened_build
%define _gprdir %_GNAT_project_dir

Name:       markdown-ada
Version:    0.1.0
Release:    git%{?dist}
Summary:    Markdown parser in Ada
Group:      Development/Libraries
License:    MIT
URL:        https://github.com/reznikmm/markdown
### Direct download is not availeble
Source0:    markdown-ada.tar.gz
BuildRequires:   gcc-gnat
BuildRequires:   fedora-gnat-project-common  >= 3
BuildRequires:   matreshka-devel
BuildRequires:   gprbuild

# gprbuild only available on these:
ExclusiveArch: %GPRbuild_arches

%description
Markdown parser library in Ada.

%package devel

Group:      Development/Libraries
License:    MIT
Summary:    Devel package for Markdown Ada
Requires:       %{name}%{?_isa} = %{version}-%{release}
Requires:   fedora-gnat-project-common  >= 2

%description devel
Devel package for markdown Ada

%prep
%setup -q -n markdown

%build
make  %{?_smp_mflags} GPRBUILD_FLAGS="%Gnatmake_optflags"

%install
rm -rf %{buildroot}
make install DESTDIR=%{buildroot} LIBDIR=%{_libdir} PREFIX=%{_prefix} GPRDIR=%{_gprdir} BINDIR=%{_bindir}

%post     -p /sbin/ldconfig
%postun   -p /sbin/ldconfig

%files
%doc LICENSES/MIT.txt
%dir %{_libdir}/markdown-ada
%{_libdir}/markdown-ada/libmarkdownada.so.%{version}
%{_libdir}/libmarkdownada.so.%{version}

%files devel
%doc README.md
%{_libdir}/markdown-ada/libmarkdownada.so
%{_libdir}/libmarkdownada.so
%{_libdir}/markdown-ada/*.ali
%{_includedir}/markdown-ada
%{_gprdir}/markdown.gpr
%{_gprdir}/manifests/markdown

%changelog
* Sat Jul 18 2020 Maxim Reznik <reznikmm@gmail.com> - 0.1.0-git
- Initial package
