var app = angular.module('genomuApp', ['ngResource','ui']);
var idle = false;
var idleNoty = null;

$(document).idle({
  onIdle: function(){
    window.idleNoty = $.noty({modal: true, modal: true, timeout: false,
                              text: 'After 10 seconds of inactivity, pausing data polling...'});
    window.idle = true;
  },
  onActive: function(){
    $.noty.close(window.idleNoty);
    window.idle = false;
  },
  idle: 10000
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

    $scope.instance = Instance.get();
    $scope.cluster = Cluster.get();
    $scope.clusterMembership = ClusterMembership.get();
    $scope.clusterMembershipPlan = ClusterMembershipPlan.get();
    $scope.clusterDiscovery = ClusterDiscovery.get();
    $scope.metrics = Metrics.get();

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
           if (v == null) v = n.ChannelResponseTime.arithmetic_mean;
           return [i + 1, v];
         });
         var pdata = $scope.metricPartitionResponseTime.map(function(v, i) {
           if (v == null) v = n.PartitionResponseTime.arithmetic_mean;
           return [i + 1, v];
         });
         var qdata = $scope.metricQuorumTime.map(function(v, i) {
           if (v == null) v = n.QuorumTime.arithmetic_mean;
           return [i + 1, v];
         });
         var cdata = $scope.metricCoordinationTime.map(function(v, i) {
           if (v == null) v = n.CoordinationTime.arithmetic_mean;
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
      otherwise({redirectTo:'/'});
});
