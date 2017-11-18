# Spiral LibIL

10/18/2017: Since I am out of good .NET interop options at the moment I am playing with the idea of making my own reader, writer and interprer for .NET IL.

Since all IL related libraries (AbsIL,Cecil) I've found so far were fairly huge, it might turn out the case that .NET interop is simply not worth it for Spiral. But I do not want to get intimidated too easily before I've tried it. My plan here will be to go trough the `.NET IL Assembler` book by Serge Lidin and try to build something up.

That will give me an rough estimate of the task's difficulty. If I can't get anything workable in under a week then that is a sure sign to drop this thing.

Programing is not without its share of risks, it too needs stop points.

I will make it my goal to parse `mscorlib`.