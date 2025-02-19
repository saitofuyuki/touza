#!/usr/bin/env python3
# Time-stamp: <2025/02/19 17:06:48 fuyuki config.py>

__doc__ = \
    """
zbt.config
~~~~~~~~~~
Configuration methods for TOUZA/zbt

:Source:     zbt/util.py
:Maintainer: SAITO Fuyuki <saitofuyuki@jamstec.go.jp>
:Created:    Oct 31 2024
"""

import sys
import numbers as nums
import pprint as ppr

import zbt.util as zu

__all__ = ['ConfigBase', 'ConfigRigid', 'ConfigFlex', ]

locallog = zu.LocalAdapter(__name__)


# ### Config
class ConfigBase():
    """Abstract base layer of class configuration."""
    names = ()
    _strip = '_'

    def prop(self, key, default=None):
        raise NotImplementedError(f"No {type(self)}.prop() method.")

    def dict_recursive(self, strip=None):
        return self._dict_recursive(type(self), strip=strip)

    @staticmethod
    def _dict_recursive(cls, strip=None):
        dd = {}
        strip = True if strip is None else False

        for c in cls.mro():
            d = {}
            for ko in sorted(c.__dict__):
                if ko.startswith(ConfigBase._strip):
                    continue
                v = c.__dict__[ko]
                if not isinstance(v, (nums.Number, tuple, list, str, dict), ):
                    continue
                k = ko
                if ko.endswith(ConfigBase._strip):
                    if strip:
                        while k.endswith(ConfigBase._strip):
                            k = k[:-1]
                    else:
                        k = k[:-1]
                        while k.endswith(ConfigBase._strip):
                            if k in d:
                                del(d[k])
                            k = k[:-1]
                        k = ko
                d[k] = c.__dict__[ko]
                #     continue
                # k = ko
                # while k in c.__dict__:
                #     k = k + ConfigBase._strip
                # k = k[:-1]
                # if not strip:
                #     ko = k
                # if ko not in d:
                #     d[ko] = c.__dict__[k]
            dd = d | dd
        # ppr.pprint(dd)
        return dd

    @staticmethod
    def parse(ref, cfg=None, pfx=None, parent=None,
              allow_bool=None, strip=None, lev=None):
        """Recursive update of config dict."""
        lev = lev or 0
        strip = strip or ''
        parent = parent or []
        cfg = cfg or {}
        pfx = pfx or ''

        def error(msg, key):
            key = '.'.join(key)
            raise ValueError(f"{pfx}[{key}] {msg}")

        for ck, cv in cfg.items():
            if cv == '':
                continue

            par = parent + [ck]
            attr = not isinstance(ref, dict)
            if attr:
                dref = ConfigBase._dict_recursive(ref, strip=False)
                # dref = ref.__dict__
            else:
                dref = ref
            st = strip
            while st:
                ak = ck + st
                if ak in dref:
                    rv = dref[ak]
                    break
                st = st[:-1]
            else:
                ak = ck
                if ak in dref:
                    rv = dref[ak]
                else:
                    error("Invalid configuration key", par)

            if (isinstance(cv, bool) or isinstance(rv, bool)) \
               and allow_bool is not False:
                # unconditional replacement with bool
                pass
            elif isinstance(rv, dict):
                if not isinstance(cv, dict):
                    error(f"Inconsistent type {type(cv)}"
                          f" >> {type(rv)}", par)
                cv = ConfigBase.parse(rv, cfg[ck], pfx=pfx,
                                      parent=par, allow_bool=allow_bool,
                                      strip=strip, lev=lev+1)
            else:
                if isinstance(rv, nums.Number):
                    if not isinstance(cv, nums.Number):
                        error(f"Inconsistent type {type(cv)}"
                              f" >> {type(rv)}", par)
                elif isinstance(rv, tuple):
                    if not isinstance(cv, list):
                        error(f"Inconsistent type {type(cv)}"
                              f" >> {type(rv)}", par)
                    if len(rv) != len(cv):
                        error(f"Inconsistent length {len(cv)}"
                              f" >> {len(rv)}", par)
                cv = type(rv)(cv)
                if locallog.is_info() and cv != rv:
                    key = '.'.join(par)
                    locallog.info(f"{pfx}[{key}] {rv} > {cv}")

            if attr:
                setattr(ref, ak, cv)
            else:
                ref[ak] = cv
        return ref

    @classmethod
    def config(cls, **kwds):
        """Dummy methods to overload."""
        raise NotImplementedError(f"No {cls}.config() method.")

    @classmethod
    def _update_config(cls, config=None, groups=None):
        """Bind class properties by config map complex."""
        config = config or {}
        groups = groups or ()
        while True:
            keys = cls.names[:]
            while keys:
                c = config.get(keys[0])
                cls.parse(cls, c, pfx=f'config[{cls}]',
                          strip=cls._strip)
                keys = keys[1:]
            if not groups:
                break
            config = config.get(groups[0]) or {}
            groups = groups[1:]

    @classmethod
    def diag(cls, *args, strip=None, **kwds):
        """Diagnose configurable class properties.
        Arguments are passed to pprint.pp()."""
        strip = zu.set_default(strip, True)
        cfg = {}
        # locallog.debug(cls.__dict__)
        # locallog.debug(ConfigBase._dict_recursive(cls))
        for k, v in ConfigBase._dict_recursive(cls, strip).items():
        # for k, v in cls.__dict__.items():
            # if k.startswith('_'):
            #     continue
            # if strip:
            #     while k[-1] == cls._strip:
            #         k = k[:-1]
            if isinstance(v, (nums.Number, tuple, list, str, dict), ):
                cfg[k] = v
        locallog.debug(f"config[{cls.__name__}]")
        ppr.pp(cfg, *args, **kwds)


class ConfigRigid(ConfigBase):
    """Rigid-type Config class, configured per class."""
    _config = False

    def prop(self, key, default=None):
        """Property inquire.  Return default if None."""
        cls = type(self)
        key = key + cls._strip
        for _ in range(len(cls._strip)):
            if hasattr(cls, key):
                ret = getattr(cls, key)
                break
            key = key[:-1]
        else:
            if hasattr(cls, key):
                ret = getattr(cls, key)
            else:
                raise ValueError(f"No property {cls}.{key}")
        if ret is None:
            ret = default
        return ret

    @classmethod
    def config(cls, config=None, groups=None):
        """Class-property configuration by complex dict."""
        if config is None:
            return
        if cls._config:
            raise ValueError(f"Double initialization in class {cls}.")
        cls._config = config
        cls._update_config(config=config, groups=groups)


class ConfigFlex(ConfigBase):
    """Flexible-type Config class, configured per instance."""

    # @classmethod
    # def config(cls, *args, **kwds):
    #     pass


def main(argv):
    """Test driver."""
    ConfigBase._dict_recursive(ConfigRigid)


if __name__ == '__main__':
    main(sys.argv[1:])
