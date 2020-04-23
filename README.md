# closhal

Транслятор кириллической формы записи в шальскую кодировку. 

Протестировать можно по адресу: 

На вход параметром подаётся нужная строка. 

В синтаксис входной строки включены: 
  + Кириллические символы, латинские *g*, *j*, *v*, *w*
  При этом:
    - *я, ё, ю* = *ьа, ьо, ьу* соответственно
    - *е, и* = *э, ы* соответственно
    - *й*  = *иь* 
    - *щ*  = *шь*
    - *ъ*  = *-*
    - *дж* = *g*
    - *дз* = *j*
    - *кс* = *v*
    - *пс* = *w*
    
    Дифтонги *дж, дз, кс, пс* автоматически преобразуются в односимвольный аналог. Чтобы они не слились, нужно разделить их точкой без пробела
    Символы, не включённые в список оставляются. Все вхождения *-* удаляются
  
    + *ь*  является символом смягчения предыдущей буквы
    + *-*  создаёт разрыв. Буква слева заканчивается, буква справа начинается. Пробела между ними не создаётся.
    + *:*  удлинняет предыдущую гласную. **Работает только после гласных, во всех остальных случаях ичезает**
    + Символ   */*   используется для разделения *аль* и *малой а*. В начале слова, в конце и отдельно */а* превращается в *малую а*. В середине используйте  *-*  после *а*
    
    
  

## License

Copyright © 2020 FIXME

This program and the accompanying materials are made available under the
terms of the Eclipse Public License 2.0 which is available at
http://www.eclipse.org/legal/epl-2.0.

This Source Code may also be made available under the following Secondary
Licenses when the conditions for such availability set forth in the Eclipse
Public License, v. 2.0 are satisfied: GNU General Public License as published by
the Free Software Foundation, either version 2 of the License, or (at your
option) any later version, with the GNU Classpath Exception which is available
at https://www.gnu.org/software/classpath/license.html.
