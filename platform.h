#ifndef __PLATFORM_H__
#define __PLATFORM_H__

#ifdef _WIN32
#   define putc_unlocked(c, stream)     _putc_nolock(c, stream)
#   define getc_unlocked(stream)        _getc_nolock(stream)
#   define flockfile(stream)            _lock_file(stream)
#   define funlockfile(stream)          _unlock_file(stream)
#endif

#endif /* __PLATFORM_H__ */
