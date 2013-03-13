var app = angular.module('genomuApp', ['ngResource','ui']);
var idle = false;
var idleNoty = null;

$(document).idle({
  onIdle: function(){
    window.idleNoty = $.noty({modal: true, modal: true, timeout: false,
                              text: 'After 30 seconds of inactivity, pausing data polling...'});
    window.idle = true;
  },
  onActive: function(){
    $.noty.close(window.idleNoty);
    window.idle = false;
  },
  idle: 30000
})

app.controller('DashboardCtrl',function($scope, $resource, $routeParams, $http, $timeout) {
    var Metrics = $resource('/metrics');
    var Instance = $resource('/instance');
    var Cluster = $resource('/cluster');
    var ClusterMembership = $resource('/cluster/membership');
    var ClusterMembershipPlan = $resource('/cluster/membership/staging',{},
                                          {commit: {method: 'POST'},
                                           discard: {method: 'DELETE'}});
    var ClusterDiscovery = $resource('/cluster/discovery');
    var Operations = $resource('/operations');

    $scope.instance = Instance.get();
    $scope.cluster = Cluster.get();
    $scope.clusterMembership = ClusterMembership.get();
    $scope.clusterMembershipPlan = ClusterMembershipPlan.get();
    $scope.clusterDiscovery = ClusterDiscovery.get();
    $scope.metrics = Metrics.get();
    $scope.operations = Operations.get();
    $scope.genomuScriptTrace = [];

    $scope.ownership = function(instance) {
      return (instance.indices.length * 100 / $scope.cluster.num_partitions)
    };
    $scope.pendingOwnership = function(instance) {
      return (instance.future_indices.length * 100 / $scope.cluster.num_partitions)
    };

    $scope.$watch('clusterMembership.instances', function(n, o) {
      if (typeof n == 'undefined' || typeof o == 'undefined') return;
      n = angular.fromJson(angular.toJson(n)).map(function(v) { return v.name });
      o = angular.fromJson(angular.toJson(o)).map(function(v) { return v.name });
      _.difference(n,o).map(function(name) {
        $.gritter.add({
          title: 'New instance',
          text: name + ' is joining the cluster',
          sticky: false
        });
      });
      _.difference(o,n).map(function(name) {
        $.gritter.add({
          title: 'Instance leaving',
          text: name + ' is leaving the cluster',
          sticky: false
        });
      });
    });

    $scope.availableInstances = function() {
      if (typeof $scope.clusterDiscovery.announced == 'undefined' ||
          typeof $scope.clusterMembership.instances == 'undefined') return [];
      discovery = angular.fromJson(angular.toJson($scope.clusterDiscovery.announced)).map(function(v) { return v.url; }).
                          filter(function(v) { return typeof(v) != 'undefined' });
      joined = angular.fromJson(angular.toJson($scope.clusterMembership.instances)).map(function(v) { return v.url; });
      return _.difference(discovery, joined);
    }

    $scope.findInstanceByURL = function(url) {
        var instances = $scope.clusterMembership.instances;
        for (var i in instances) {
            if (instances[i].url == url) {
                return instances[i]
            }
        }
        var instances = $scope.clusterDiscovery.announced;
        for (var i in instances) {
            if (instances[i].url == url) {
                return instances[i]
            }
        }
        return null;
    }

    $scope.commitPlan = function() {
        $scope.clusterMembershipPlan.$commit();
    }

    $scope.discardPlan = function() {
        $scope.clusterMembershipPlan.$discard(); 
    }

    $scope.stageJoin = function() {
       var i = $scope.instanceToAdd;
       if (i.substring(0, 6) != "http://" &&
           i.substring(0, 7) != "https://") {
          i = "http://" + i;
       }
       $scope.instanceToAdd = '';
       $scope.join(i);
    }

    $scope.join = function(i) {
       $http.jsonp(i +
                   '/cluster/membership',
                   {params: {instance_url: $scope.instance.instance_url, method: 'POST'}});
    }

    if ($("#metric-ProcessingTime").length > 0) {
      $scope.metricChannelResponseTime = [];
      $scope.metricPartitionResponseTime = [];
      $scope.metricQuorumTime = [];
      $scope.metricCoordinationTime = [];

      for (var i = 0; i < 20; i++) {
        $scope.metricChannelResponseTime.push(null);
        $scope.metricPartitionResponseTime.push(null);
        $scope.metricQuorumTime.push(null);
        $scope.metricCoordinationTime.push(null);
      }

      $scope.graphProcessingTime = $.plot($("#metric-ProcessingTime"), [{label: "Channel Response Time (Arithmetic Mean)", data: []},
                                                                        {label: "Coordination Time (Arithmetic Mean)", data: []},
                                                                        {label: "Quorum Time (Arithmetic Mean)", data: []},
                                                                        {label: "Partition Response Time (Arithmetic Mean)", data: []},
                                                                        ], {
        series: { shadowSize: 1 },
        lines: { show: true, lineWidth: 3, fill: true, fillColor: { colors: [ { opacity: 0.4 }, { opacity: 0.4 } ] }},        
        yaxis: { show: true, min: 0, tickFormatter: function (v) { return v + " µs"; }},
        xaxis: { show: false, min: 0, max: 20 },
        colors: ["#FA5833"],
        legend: { position: "sw" },
        grid: { tickColor: "#f9f9f9",
            borderWidth: 0,
        }
      });

      var opts = $scope.graphProcessingTime.getOptions();
      opts.yaxes.forEach(function(yaxis, i) {
        var originalFormatter = yaxis.tickFormatter || function(v) { return v; }
        yaxis.tickFormatter = function(v) {
          return '<span style="color: ' + opts.colors[i] + '">' + originalFormatter(v) + '</span>';
        }
      });


      $scope.$watch('metrics', function(n) {
         if (typeof n.ChannelResponseTime == 'undefined') return;
         $scope.metricChannelResponseTime.push(n.ChannelResponseTime.arithmetic_mean);
         $scope.metricChannelResponseTime.shift();
         $scope.metricPartitionResponseTime.push(n.PartitionResponseTime.arithmetic_mean);
         $scope.metricPartitionResponseTime.shift();
         $scope.metricQuorumTime.push(n.QuorumTime.arithmetic_mean);
         $scope.metricQuorumTime.shift();
         $scope.metricCoordinationTime.push(n.CoordinationTime.arithmetic_mean);
         $scope.metricCoordinationTime.shift();

         var opts = $scope.graphProcessingTime.getOptions();
         opts.yaxis.max = Math.max.apply(this, $scope.metricChannelResponseTime.
                                               concat($scope.metricPartitionResponseTime).
                                               concat($scope.metricQuorumTime).
                                               concat($scope.metricCoordinationTime).
                                               filter(function(v) { return v != null }));

         var data = $scope.metricChannelResponseTime.map(function(v, i) {
           if (v == 0) v = null;
           return [i + 1, v];
         });
         var pdata = $scope.metricPartitionResponseTime.map(function(v, i) {
           if (v == 0) v = null;
           return [i + 1, v];
         });
         var qdata = $scope.metricQuorumTime.map(function(v, i) {
           if (v == 0) v = null;
           return [i + 1, v];
         });
         var cdata = $scope.metricCoordinationTime.map(function(v, i) {
           if (v == 0) v = null;
           return [i + 1, v];
         });

         $scope.graphProcessingTime.setData([ {label: "Channel Response Time (Arithmetic Mean)", data: data},
                                              {label: "Coordination Time (Arithmetic Mean)", data: cdata},
                                              {label: "Quorum Time (Arithmetic Mean)", data: qdata},
                                              {label: "Partition Response Time (Arithmetic Mean)", data: pdata},
                                         ]);
         $scope.graphProcessingTime.setupGrid();
         $scope.graphProcessingTime.draw();
      }, true);
    }


    if ($("#metric-System").length > 0) {
      $scope.metricMemory = [];
      $scope.metricLoadAvg = [];
      $scope.metricUtil = [];
      $scope.metricProcesses = [];

      for (var i = 0; i < 20; i++) {
        $scope.metricMemory.push(null);
        $scope.metricLoadAvg.push(null);
        $scope.metricUtil.push(null);
        $scope.metricProcesses.push(null);
      }

      $scope.graphSystem = $.plot($("#metric-System"), [{label: "Available Memory", data: []},
                                                        {label: "Load Avg", data: []},
                                                        {label: "CPU Utilization", data: []},
                                                        ], {
        series: { shadowSize: 10 },
        yaxes: [{ position: 'left', show: true, min: 0, tickFormatter: function (v) { return v + " Mb"; }},
                { position: 'left', show: true, min: 0, tickFormatter: function (v) { return v.toFixed(2); }},
                { position: 'right', show: true, min: 0, max: 100},
                { position: 'right', show: true, min: 0},
                ],
        xaxis: { show: false, min: 0, max: 20 },
        colors: ["#FA5833"],
        legend: { position: "sw" },
        grid: { tickColor: "#f9f9f9",
            borderWidth: 0,
        }
      });

      var opts = $scope.graphSystem.getOptions();
      opts.yaxes.forEach(function(yaxis, i) {
        var originalFormatter = yaxis.tickFormatter || function(v) { return v; }
        yaxis.tickFormatter = function(v) {
          return '<span style="color: ' + opts.colors[i] + '">' + originalFormatter(v) + '</span>';
        }
      });


      $scope.$watch('metrics', function(n) {
         if (typeof n.Memory == 'undefined' || typeof n.CPU == 'undefined') return;
         $scope.metricMemory.push(n.Memory.free_memory / 1024 / 1024);
         $scope.metricMemory.shift();
         $scope.metricLoadAvg.push(n.CPU.avg1 / 256);
         $scope.metricLoadAvg.shift();
         $scope.metricUtil.push(n.CPU.utilization);
         $scope.metricUtil.shift();
         $scope.metricProcesses.push(n.Processes);
         $scope.metricProcesses.shift();

         var opts = $scope.graphSystem.getOptions();
         opts.yaxes[0].max = Math.max.apply(this, $scope.metricMemory.
                                                  filter(function(v) { return v != null }));
         opts.yaxes[1].max = Math.max.apply(this, $scope.metricLoadAvg.
                                                  filter(function(v) { return v != null }));

         opts.yaxes[3].max = Math.max.apply(this, $scope.metricProcesses.
                                                  filter(function(v) { return v != null }));

         var mdata = $scope.metricMemory.map(function(v, i) {
           if (v == 0) v = null;
           return [i + 1, v];
         });
         var ladata = $scope.metricLoadAvg.map(function(v, i) {
           if (v == 0) v = null;
           return [i + 1, v];
         });
         var udata = $scope.metricUtil.map(function(v, i) {
           if (v == 0) v = null;
           return [i + 1, v];
         });
         var pdata = $scope.metricProcesses.map(function(v, i) {
           if (v == 0) v = null;
           return [i + 1, v];
         });

         $scope.graphSystem.setData([{label: "Available Memory", data: mdata, yaxis: 1},
                                     {label: "Load Avg", data: ladata, yaxis: 2},
                                     {label: "CPU Utilization", data: udata, yaxis: 3},
                                     {label: "Processes", data: pdata, yaxis: 4},
                                    ]);
         $scope.graphSystem.setupGrid();
         $scope.graphSystem.draw();
      }, true);
    }

    $scope.allOperations = function() {
      var ops = [];
      for (var mod in $scope.operations) {
        for (var i in $scope.operations[mod].operations) {
          var op = $scope.operations[mod].operations[i];
          op.module = mod;
          op.module_id = $scope.operations[mod].id;
          ops.push(op);
        }
      }
      return ops;
    }

    var Genomu = function() {
      var api = function(arities, module_id) {
        return function() {
          var operation_id = arities[arguments.length];
          var args = Array.prototype.slice.call(arguments);
          var arg = args;
          if (args.length == 0) {
            arg = null;
          } else if (args.length == 1) {
            arg = args[0];
          }
          return {'_genomu': true, module: module_id, operation: operation_id, arity: args.length, argument: arg};
        };
      }
      var modules = [];
      this._modules = {};
      for (var k in $scope.operations) {
        if (typeof $scope.operations[k] == 'object')
          modules.push($scope.operations[k]);
      }
      for (var i in modules) {
        this[modules[i].name] = {};
        this._modules[modules[i].id] = {};
        var operations = modules[i].operations;
        var me = this;
        operations = operations.reduce(function(acc, op) {
            acc[op.name] = acc[op.name] || {arities: {}};
            acc[op.name].arities[op.args] = op.id;
            me._modules[modules[i].id][[op.id, op.args]] = [modules[i].name, op.name];
            return acc;
        }, {});
        for (var j in operations) {
          this[modules[i].name][j] = api(operations[j].arities, modules[i].id);
        }
      }
      this.set = function(key, operation, cb) {
        actual_key = key;
        if (_.isString(actual_key)) actual_key = [key];
        if (!(typeof operation == 'object' && operation['_genomu'] == true)) {
          operation = this.core.identity(operation);
        }
        var me = this;
        $http.post("/operations/" + this.channel,
                  {key: actual_key, command: 1, operation: operation}).
        success(function(data, status) {
          $scope.genomuScriptTrace.push({type: 'set', data: data, operation: operation, key: key, instance: me});
          if (typeof cb == 'function') {
            cb.apply(this, [data]);
          }
        });
      };
      this.apply = function(key, operation, cb) {
        actual_key = key;
        if (_.isString(actual_key)) actual_key = [key];
        var me = this;
        $http.post("/operations/" + this.channel,
                  {key: actual_key, command: 2, operation: operation}).
        success(function(data, status) {
          $scope.genomuScriptTrace.push({type: 'apply', data: data, operation: operation, key: key, instance: me});
          if (typeof cb == 'function') {
            cb.apply(this, [data]);
          }
        });
      };
      this.commit = function(cb) {
        var me = this;
        $http.post("/operations/" + this.channel,
                  {commit: true}).
        success(function(data, status) {
          $scope.genomuScriptTrace.push({type: 'commit', instance: me});
          if (typeof cb == 'function') {
            cb.apply(this, [data]);
          }
        });
      };
      this.get = function(key, operation, cb) {
        if (typeof operation == 'function') cb = operation;
        if (typeof operation == 'undefined') operation = this.core.identity(); 
        actual_key = key;
        if (_.isString(actual_key)) actual_key = [key];
        var me = this;
        $http.post("/operations/" + this.channel,
                  {key: actual_key, command: 0, operation: operation}).
        success(function(data, status) {
          $scope.genomuScriptTrace.push({type: 'get', data: data, operation: operation, key: key, instance: me});
          if (typeof cb == 'function') {
            cb.apply(this, [data]);
          }
        });
      };
      this.inspect = function(value) {
        if (typeof value == 'object' && value['_genomu'] == true) {
          info = this._modules[value.module][[value.operation, value.arity]];
          if (value.argument == null) {
            return info[0] + "." + info[1] + "()";  
          } else {
            return info[0] + "." + info[1] + "(" + this.inspect(value.argument) + ")";
          }      
        } else {
          return '' + value;
        }
      }

    }

    $scope.evalScript = function() {
       var genomu = new Genomu();
       $scope.genomuScriptTrace = [];
       $http.post("/operations").
         success(function(data, status) {
            genomu.channel = data.channel 
            try {
              eval("(function() { " + $scope.genomuScript + "})()");
            } catch(e) {
              noty({layout: 'center', type: 'error', text: 'Evaluation error: ' + e})
            }
         });
    }

    $scope.page = function() {
      return window.location.pathname + window.location.hash;
    }

    var refresh = function() {
      $scope.instance.$get();
      $scope.cluster.$get();
      $scope.clusterMembership.$get();
      $scope.clusterMembershipPlan.$get();
      $scope.clusterDiscovery.$get();
      $scope.metrics.$get();
    }

    var refresh_ = function() {
        if (!window.idle) refresh();
        $timeout(refresh_, 2500);
    }

    refresh_();

    $scope.$console = window.console;
});

app.config(function ($routeProvider) {
      $routeProvider.
      when('/', { templateUrl: '/template/dashboard', controller: 'DashboardCtrl' }).
      when('/instances', { templateUrl: '/template/instances', controller: 'DashboardCtrl' }).
      when('/partitions', { templateUrl: '/template/partitions', controller: 'DashboardCtrl' }).
      when('/explorer', { templateUrl: '/template/explorer', controller: 'DashboardCtrl' }).
      otherwise({redirectTo:'/'});
});
