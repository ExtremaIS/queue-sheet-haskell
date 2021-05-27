Name:          queue-sheet-haskell
Version:       {{VERSION}}
Release:       1%{?dist}
Summary:       Queue sheet utility
License:       MIT
URL:           https://github.com/ExtremaIS/queue-sheet-haskell
Source0:       queue-sheet-haskell-{{VERSION}}.tar.xz
BuildArch:     {{ARCH}}
BuildRequires: make
Requires:      glibc,gmp
#ExcludeArch:

%description
Queue Sheet is a utility that builds PDFs of lists that can be used to track
progress when offline.

%global debug_package %{nil}

%prep
%setup -q

%build

%install
make install DESTDIR=%{buildroot} PREFIX=/usr

%check
make test

%files
%{_bindir}/queue-sheet
%{_mandir}/man1/queue-sheet.1.gz
%{_datadir}/doc/%{name}/

%changelog
* {{DATE}} {{RPMFULLNAME}} <{{RPMEMAIL}}> - {{VERSION}}-1
- Release {{VERSION}}
