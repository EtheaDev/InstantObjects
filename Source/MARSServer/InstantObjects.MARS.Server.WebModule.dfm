object ServerWebModule: TServerWebModule
  OnCreate = WebModuleCreate
  OnDestroy = WebModuleDestroy
  Actions = <
    item
      Default = True
      Name = 'DefaultHandler'
      PathInfo = '/'
      OnAction = ServerWebModuleDefaultHandlerAction
    end>
  Height = 230
  Width = 415
end
