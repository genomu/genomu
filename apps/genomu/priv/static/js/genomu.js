var app = angular.module('genomuApp', ['ngResource','ui']);
var idle = false;

$(document).idle({
  onIdle: function(){
    window.idle = true;
  },
  onActive: function(){
    window.idle = false;
  },
  idle: 10000
})

app.controller('DashboardCtrl',function($scope, $resource, $routeParams, $http, $timeout) {
    var Instance = $resource('/instance');
    var Cluster = $resource('/cluster');
    var ClusterMembership = $resource('/cluster/membership');
    var ClusterMembershipPlan = $resource('/cluster/membership/staging',{},
                                          {commit: {method: 'POST'},
                                           discard: {method: 'DELETE'}});

    $scope.instance = Instance.get();
    $scope.cluster = Cluster.get();
    $scope.clusterMembership = ClusterMembership.get();
    $scope.clusterMembershipPlan = ClusterMembershipPlan.get();

    $scope.ownership = function(instance) {
      return (instance.indices.length * 100 / $scope.cluster.num_partitions)
    };
    $scope.pendingOwnership = function(instance) {
      return (instance.future_indices.length * 100 / $scope.cluster.num_partitions)
    };

    $scope.findInstanceByURL = function(url) {
        var instances = $scope.clusterMembership.instances;
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

       $http.jsonp(i +
                   '/cluster/membership',
                   {params: {instance_url: $scope.instance.instance_url, method: 'POST'}});

        $scope.instanceToAdd = '';
    }

    $scope.page = function() {
      return window.location.pathname + window.location.hash;
    }

    var refresh = function() {
      $scope.instance.$get();
      $scope.cluster.$get();
      $scope.clusterMembership.$get();
      $scope.clusterMembershipPlan.$get();
    }

    var refresh_ = function() {
        if (!window.idle) refresh();
        $timeout(refresh_, 2500);
    }

    refresh_();

});

app.config(function ($routeProvider) {
      $routeProvider.
      when('/', { templateUrl: '/template/dashboard', controller: 'DashboardCtrl' }).
      when('/instances', { templateUrl: '/template/instances', controller: 'DashboardCtrl' }).
      when('/partitions', { templateUrl: '/template/partitions', controller: 'DashboardCtrl' }).
      otherwise({redirectTo:'/'});
});
