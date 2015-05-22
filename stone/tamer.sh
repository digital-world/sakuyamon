#!/bin/sh

function syslots {
    read a;
    test $? -eq 0 && read b;
    while test $? -eq 0; do
        test `expr $b - $a` -ne 1 && echo `expr $a + 1`;
        read a;
        test $? -eq 0 && read b;
    done
    test -n "$a" && echo `expr $a + 1`;
}

function dstamer {
    case "$1" in
        create)
            dscl . read /Groups/$2;
            if test $? -ne 0; then
                sysid=`dscl . list Groups PrimaryGroupID | awk '$2 > 0 {print $2}' | sort -n | syslots | head -n 1`;
                dseditgroup -o create -i ${sysid} -r "Digimon Tamers" $2;
            fi
            dscl . read /Users/$2;
            if test $? -ne 0; then
                sysid=`dscl . list Users UniqueID | awk '$2 > 0 {print $2}' | sort -n | syslots | head -n 1`;
                dscl . create /Users/$2;
                dscl . create /Users/$2 UniqueID ${sysid};
                dscl . create /Users/$2 RealName "Digimon Tamer Daemon";
                dscl . create /Users/$2 NFSHomeDirectory /var/empty;
                dscl . create /Users/$2 PrimaryGroupID `dscl . read /Groups/$2 PrimaryGroupID | tr -d '[:alpha:]:'`;
                dscl . create /Users/$2 UserShell `which nologin`;
            fi
            ;;
        delete)
            dscl . delete /Users/$2;
            dseditgroup -o delete $2;
            ;;
        *)
            false;
            ;;
    esac
}

function modtamer {
    case "$1" in
        create)
            getent group $2 || groupadd -r $2;
            getent passwd $2 || useradd -r -M -g $2 -s `which nologin` -c "Digimon Tamer Daemon" $2;
            ;;
        delete)
            getent passwd $2 && userdel -r $2;
            getent group $2 && groupdel $2;
            ;;
        *)
            false;
            ;;
    esac
}

case "$1" in
    macosx)
        dstamer $2 tamer;
        ;;
    unix)
        modtamer $2 tamer;
        ;;
    *)
        false;
        ;;
esac
