# Famulus Doc Generator
# encoding: utf-8

$proj_folder = "P:/Atalan/"

def load_file(filename)
# Read text file (eventually removing UTF-8 BOM chars and return it as list of lines
  arr = IO.readlines(filename, :encoding => 'bom|UTF-8')
  if arr[0] =~ /\A\xEF\xBB\xBF(.*)/
    arr[0] = $1
  end
  return arr
end

def put_file(filename)
  head = IO.readlines($proj_folder + "www_src/" + filename) 
  head.each{|line|
    puts line
  }
end

def html_file(filename)
  put_file("header.html")
  put_file(filename)
  put_file("footer.html") 
end

def generate_index(filename)

  arr = IO.readlines($proj_folder + filename)
  
  heading_level = 0
  lv = [0, 0, 0, 0]
  first = true
  text = "<div id=\"index\">\n"
  arr.each_with_index { |line, index|  
    if line =~ /\*\*+/ 
      if heading_level == 1
        heading_level = 0
        first = false
      else
        heading_level = 1
      end
    elsif line =~ /\A\=\=+/ 
      if heading_level == 2
        heading_level = 0
      else
        heading_level = 2
      end
    else
      if heading_level > 0 and not first
        lv[heading_level] = lv[heading_level] + 1
        text = text + "<h#{heading_level}><a href=\"\##{lv[1]}_#{lv[2]}\">#{line.strip}</a></h#{heading_level}>\n"
      end
    end
  }
  text = text + "</div>\n"
  return text
end

def put_text(text)
  #::text:: is code word
  text.gsub!(/::(.+?)::/,  '<code>\1</code>')  
  puts text
end

def a_file(filename, in_comment)

  index_text = generate_index(filename)
  
  arr = IO.readlines($proj_folder + filename)
  
  lv = [0, 0, 0, 0]
  in_list = false
  in_code = false
  first = true
  
  heading_level = 0
  arr.each_with_index { |line, index|
    # Non Comment section
    if not in_comment
      if line =~ /\A\/\*\Z/
        in_comment = true
      end
    # Comment section
    else
      if line.strip == "::index::"
        puts index_text
      elsif line =~ /\A\*\/\Z/
        in_comment = false
      elsif line =~ /\A:::::+\s*\Z/
        if not in_code
          puts "<code>"
          in_code = true
        else
          puts "</code>"
          in_code = false
        end
          
      elsif line =~ /\A\s*\-/
        if not in_list then
          puts "<ul>"
          in_list = true
        end
        mt = line.match(/\A\s*-\s*(.*)\Z/)
        puts '<li>'
        put_text(mt[1])
        puts '</li>'
      elsif line =~ /\*\*+/ 
        if heading_level == 1
          heading_level = 0
          first = false
        else
          heading_level = 1
        end
      elsif line =~ /\A\=\=+/ 
        if heading_level == 2
          heading_level = 0
        else
          heading_level = 2
        end
      elsif line =~ /Purpose:/
        #in header file, two lines back should be function declaration like "typedef void   (*DataFreeFn)   (VoidPtr data);"
        mt = arr[index-2].match(/typedef\s+(\w+)\s+\(\*(.+)Fn\)\s*\((.*)\);/)
        puts
        puts  "<h3 class=\"func\">#{mt[1]} <b>#{mt[2]}</b>(#{mt[3]})</h3>"
        puts
      elsif line =~ /\A\s*\Z/
          if in_list then
            in_list = false
            puts "</ul>"
          else
          	if in_code then
            	puts ""
            else
            	puts "</p><p>"
            end
          end
      else
        if heading_level > 0
          if first
            puts "<h#{heading_level}>#{line.strip}</h#{heading_level}>"
          else
            lv[heading_level] = lv[heading_level] + 1
            puts "<a name=\"#{lv[1]}_#{lv[2]}\"><h#{heading_level}>#{line.strip}</h#{heading_level}>"
          end
        else
          if in_code then
            puts line
          else
            put_text line.strip
          end
          
        end
      end
    end    
  }

  
end

def o_file(filename, in_comment)
  #Generate header
  put_file("header.html")

  a_file(filename, in_comment)
  #Generate footer  
  put_file("footer.html") 
end

def h_file(filename)
    o_file(filename, false)
end

def  txt_file(filename)
  o_file("doc/" + filename, true)
end

def atalan_line(line)
  if line =~ /(\s*\A;.*)/ 
    line = "<i>" + line + "</i>"
  else
    line.gsub!(/\//, '<b>/</b>')
    line.gsub!(/\(/, '<b>(</b>')
    line.gsub!(/\)/, '<b>)</b>')
    line.gsub!(/out/, '<b>out</b>')
    line.gsub!(/(in)\s/, '<b>in</b>')
    line.gsub!(/const/, '<b>const</b>')
    line.gsub!(/for/, '<b>for</b>')
    line.gsub!(/if/, '<b>if</b>')
    line.gsub!(/else/, '<b>else</b>')
    line.gsub!(/then/, '<b>then</b>')
    line.gsub!(/step/, '<b>step</b>')
    line.gsub!(/where/, '<b>where</b>')
    line.gsub!(/while/, '<b>while</b>')
    line.gsub!(/until/, '<b>until</b>')
    line.gsub!(/array/, '<b>array</b>')
    line.gsub!(/type/, '<b>type</b>')
    line.gsub!(/file/, '<b>file</b>')
    line.gsub!(/inc/, '<b>inc</b>')
    line.gsub!(/dec/, '<b>dec</b>')

    line.gsub!(/sqrt/, '<b>sqrt</b>')
    line.gsub!(/\b(and)/, '<b>and</b>')
    line.gsub!(/\b(or)/, '<b>or</b>')
    line.gsub!(/\b(not)/, '<b>not</b>')

    line.gsub!(/\b(bitand)/, '<b>bitand</b>')
    line.gsub!(/\b(bitor)/, '<b>bitor</b>')
    line.gsub!(/\b(bitnot)/, '<b>bitnot</b>')

    line.gsub!(/=/, '<b>=</b>')
    line.gsub!(/\.\./, '<b>..</b>')
    line.gsub!(/\+/, '<b>+</b>')
    line.gsub!(/\-/, '<b>-</b>')
    line.gsub!(/\*/, '<b>*</b>')

  end
end

def ata_example(name, filenames)
  puts("<h2>#{name}</h2>")
  puts("<p>")
  a_file("examples/" + name + "/readme.txt", true)
  puts("</p>")
  puts "<table>"
  filenames.each { |filename1|    
    filename = name + "/" + filename1
    puts "<tr>"
    puts "  <td><a href=\"examples/#{filename}.html\">#{filename1}</a></td><td><a href=\"examples/#{filename}.atl\">#{filename1}.atl</a></td>"
    puts "  <td><a href=\"examples/#{filename}.asm\">#{filename1}.asm</a></td>"
    puts "  <td><a href=\"examples/#{filename}.xex\">#{filename1}.xex</a></td>"
    puts "</tr>"
  }
  
  puts "</table>"
  
 filenames.each { |filename1|
    filename = name + "/" + filename1
    f = load_file("examples/#{filename}.atl")
    $stdout = File.new($proj_folder + "www/examples/" + filename + ".html", "w")
puts <<XXXX
<html>
<head>
<META HTTP-EQUIV="CONTENT-TYPE" CONTENT="text/html; charset=utf-8">
<link rel=StyleSheet type="text/css"
      href="../template/main.css">
<title>Atalan example: #{filename}.atl</title>
</head>
<body>
<pre class='code'>
XXXX

f.each { |line| 	
  puts atalan_line(line)
}


puts <<XXXX
</pre>
</body>
</html>
XXXX
  }
   
end

$stdout = File.new($proj_folder + 'www/index.html', 'w')
txt_file "index.txt"

$stdout = File.new($proj_folder + 'www/contact.html', 'w')
txt_file "contact.txt"

$stdout = File.new($proj_folder + 'www/reference.html', 'w')
txt_file "reference.txt"

$stdout = File.new($proj_folder + 'www/howto.html', 'w')
txt_file "howto.txt"

$stdout = File.new($proj_folder + 'www/examples.html', 'w')
put_file("header.html")
puts("<h1>Examples</h1>")

#ata_example("Atari", ["hello_font"])
ata_example("Atari", ["hello_font","rainbow","esieve","count_2sec","stars","interrupts","pmg"])
put_file("footer.html") 


$stdout = File.new($proj_folder + 'www/tutorial.html', 'w')
txt_file "tutorial.txt"

$stdout = File.new($proj_folder + 'www/usage.html', 'w')
txt_file "usage.txt"

$stdout = File.new($proj_folder + 'www/optim.html', 'w')
txt_file "optim.txt"

$stdout = File.new($proj_folder + 'www/backend.html', 'w')
txt_file "backend.txt"

$stdout = File.new($proj_folder + 'www/atari.html', 'w')
txt_file "platform_atari.txt"

$stdout = File.new($proj_folder + 'www/atmos.html', 'w')
txt_file "platform_atmos.txt"

$stdout = File.new($proj_folder + 'www/nes.html', 'w')
txt_file "platform_nes.txt"

$stdout = File.new($proj_folder + 'www/c64.html', 'w')
txt_file "platform_c64.txt"

$stdout = File.new($proj_folder + 'www/con6502.html', 'w')
txt_file "platform_con6502.txt"

$stdout = File.new($proj_folder + 'www/sim6502.html', 'w')
txt_file "platform_sim6502.txt"

$stdout = File.new($proj_folder + 'www/news.html', 'w')
txt_file "news.txt"

$stdout = File.new($proj_folder + 'www/projects.html', 'w')
txt_file "projects.txt"

$stdout = File.new($proj_folder + 'www/m6502.html', 'w')
o_file("src/atalan/cpu/m6502/m6502.txt", true)

$stdout = File.new($proj_folder + 'www/Z80.html', 'w')
o_file("src/atalan/cpu/Z80/Z80.txt", true)

$stdout = File.new($proj_folder + 'www/zxspectrum.html', 'w')
o_file("src/atalan/platform/ZXSpectrum/ZXSpectrum.txt", true)

$stdout = File.new($proj_folder + 'www/conZ80.html', 'w')
o_file("src/atalan/platform/conZ80/conZ80.txt", true)
