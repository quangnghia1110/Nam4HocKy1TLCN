package studentConsulting.controller.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.payload.dto.department_field.ManageDepartmentDTO;
import studentConsulting.model.payload.request.department_field.DepartmentRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminDepartmentService;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonPdfService;

import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AdminDepartmentController {

    @Autowired
    private IAdminDepartmentService departmentService;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/department/list")
    public ResponseEntity<DataResponse<Page<ManageDepartmentDTO>>> getDepartments(
            @RequestParam(required = false) String name,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageDepartmentDTO> departments = departmentService.getAllDepartmentsWithFilters(Optional.ofNullable(name), pageable);

        if (departments.isEmpty()) {
            return ResponseEntity.status(404).body(
                    new DataResponse<>("error", "Không tìm thấy phòng ban phù hợp")
            );
        }

        return ResponseEntity.ok(
                new DataResponse<>("success", "Lấy danh sách phòng ban thành công", departments)
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/department/create")
    public ResponseEntity<DataResponse<ManageDepartmentDTO>> createDepartment(@RequestBody DepartmentRequest departmentRequest) {
        if (departmentRequest == null || departmentRequest.getName().trim().isEmpty()) {
            return ResponseEntity.status(400).body(
                    new DataResponse<>("error", "Dữ liệu phòng ban không hợp lệ")
            );
        }

        ManageDepartmentDTO savedDepartment = departmentService.createDepartment(departmentRequest);

        return ResponseEntity.ok(
                new DataResponse<>("success", "Tạo phòng ban thành công", savedDepartment)
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/department/update")
    public ResponseEntity<DataResponse<ManageDepartmentDTO>> updateDepartment(@RequestParam Integer id, @RequestBody DepartmentRequest departmentRequest) {
        if (departmentRequest == null || departmentRequest.getName().trim().isEmpty()) {
            return ResponseEntity.status(400).body(
                    new DataResponse<>("error", "Dữ liệu phòng ban không hợp lệ")
            );
        }

        ManageDepartmentDTO updatedDepartment = departmentService.updateDepartment(id, departmentRequest);

        return ResponseEntity.ok(
                new DataResponse<>("success", "Cập nhật phòng ban thành công", updatedDepartment)
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/admin/department/delete")
    public ResponseEntity<DataResponse<Void>> deleteDepartment(@RequestParam Integer id) {
        try {
            departmentService.deleteDepartmentById(id);
            return ResponseEntity.ok(
                    new DataResponse<>("success", "Xóa phòng ban thành công")
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    new DataResponse<>("error", "Không tìm thấy phòng ban để xóa")
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/department/detail")
    public ResponseEntity<DataResponse<ManageDepartmentDTO>> getDepartmentById(@RequestParam Integer id) {
        try {
            ManageDepartmentDTO manageDepartmentDTO = departmentService.getDepartmentById(id);
            return ResponseEntity.ok(
                    new DataResponse<>("success", "Lấy thông tin phòng ban thành công", manageDepartmentDTO)
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    new DataResponse<>("error", "Không tìm thấy phòng ban")
            );
        }
    }
}

