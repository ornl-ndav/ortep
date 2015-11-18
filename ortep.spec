Name:           ortep
Version:        1.0.3
Release:        1%{?dist}
Summary:        Oak Ridge Thermal Ellipsoid Plot Program for Crystal Structure Illustrations
Group:          Development/Tools
Vendor:         Carroll K. Johnson <ckj@ornl.gov>
Packager:       Peter F. Peterson <petersonpf@ornl.gov>

License:        unknown
URL:            https://github.com/ornl-ndav/ortep/
Source0:        %{name}-%{version}.tar.gz
#Source0:        http://www.github.com/ornl-dav/ortep/releases/download/v%{version}/%{name}-%{version}.tar.gz

BuildRequires:  gcc-gfortran pgplot-devel
#Prefix:         /usr/local

%description


%prep
%setup -q


%build
%{!?make_build: %define make_build %{__make} %{?_smp_mflags}}
%make_build

%install
rm -rf $RPM_BUILD_ROOT
mkdir -p $RPM_BUILD_ROOT/usr/bin
%make_install

#%check
#ctest

%files
%defattr(-,root,root,-)
%{_prefix}/bin/ortep3
%doc
