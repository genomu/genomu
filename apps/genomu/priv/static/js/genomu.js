var app = angular.module('genomuApp', ['ngResource']);


app.controller('DashboardCtrl',function($scope, $resource, $routeParams) {
    var Instance = $resource('/instance');
    var Cluster = $resource('/cluster');
    var ClusterMembership = $resource('/cluster/membership');
    var ClusterMembershipPlan = $resource('/cluster/membership/staging');
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

});

app.config(function ($routeProvider) {
      $routeProvider.
      when('/', { templateUrl: '/template/dashboard', controller: 'DashboardCtrl' }).
      when('/instances', { templateUrl: '/template/instances', controller: 'DashboardCtrl' }).
      when('/partitions', { templateUrl: '/template/partitions', controller: 'DashboardCtrl' }).
      otherwise({redirectTo:'/'});
});
