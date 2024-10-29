package studentConsulting.controller.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import studentConsulting.model.payload.dto.statistic.AdminStatisticsDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminStatisticsService;

import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("${base.url}")
public class AdminStatisticsController {

    @Autowired
    private IAdminStatisticsService statisticsService;

    @PreAuthorize("hasRole('ADMIN')")
    @GetMapping("/admin/statistics")
    public ResponseEntity<DataResponse<AdminStatisticsDTO>> getAdminStatistics() {
        AdminStatisticsDTO data = statisticsService.getAllAdminStatistics();
        return ResponseEntity.ok(DataResponse.<AdminStatisticsDTO>builder()
                .status("success")
                .message("Lấy thống kê thành công")
                .data(data)
                .build());
    }

    @PreAuthorize("hasRole('ADMIN')")
    @GetMapping("/admin/statistics/accounts/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getAccountsByYear(@RequestParam int year) {
        List<Map<String, Object>> data = statisticsService.getAccountsByYear(year);
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy thống kê tài khoản theo năm thành công.")
                .data(data)
                .build());
    }

    @PreAuthorize("hasRole('ADMIN')")
    @GetMapping("/admin/statistics/departments/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getDepartmentsByYear(@RequestParam int year) {
        List<Map<String, Object>> data = statisticsService.getDepartmentsByYear(year);
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy thống kê phòng ban theo năm thành công.")
                .data(data)
                .build());
    }

    @PreAuthorize("hasRole('ADMIN')")
    @GetMapping("/admin/statistics/fields/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getFieldsByYear(
            @RequestParam int year, @RequestParam(required = false) Integer departmentId) {
        List<Map<String, Object>> data = statisticsService.getFieldsByYear(departmentId, year);
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy thống kê lĩnh vực theo năm thành công.")
                .data(data)
                .build());
    }

    @PreAuthorize("hasRole('ADMIN')")
    @GetMapping("/admin/statistics/role-asks/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getRoleAsksByYear(@RequestParam int year) {
        List<Map<String, Object>> data = statisticsService.getRoleAsksByYear(year);
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy thống kê yêu cầu vai trò theo năm thành công.")
                .data(data)
                .build());
    }

    @PreAuthorize("hasRole('ADMIN')")
    @GetMapping("/admin/statistics/role-consultants/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getRoleConsultantsByYear(@RequestParam int year) {
        List<Map<String, Object>> data = statisticsService.getRoleConsultantsByYear(year);
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy thống kê vai trò tư vấn theo năm thành công.")
                .data(data)
                .build());
    }

    @PreAuthorize("hasRole('ADMIN')")
    @GetMapping("/admin/statistics/roles/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getRolesByYear(@RequestParam int year) {
        List<Map<String, Object>> data = statisticsService.getRolesByYear(year);
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy thống kê vai trò theo năm thành công.")
                .data(data)
                .build());
    }
}
