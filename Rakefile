task :clean do
  files_to_clean = File.read('.gitignore').split("\n").map { |i| "**/#{i}"}
  p files_to_clean
  Dir.glob(files_to_clean).each do |file|
    rm file
  end
end

task :default do
  system("ocaml install.ml")
end