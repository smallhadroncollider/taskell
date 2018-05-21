# config valid only for current version of Capistrano
set :application, 'taskell.app'
set :repo_url, 'git@github.com:smallhadroncollider/build--taskell.app.git'

append :linked_dirs, "public/bottles"

namespace :deploy do
    task :mod_group do
        on roles(:app) do
            execute "sudo chown -R mark:www-data #{deploy_to}"
            execute "sudo chmod -R 775 #{deploy_to}"
            info "Adding www-data permissions"
        end
    end

    after :updated, "deploy:mod_group"
end
