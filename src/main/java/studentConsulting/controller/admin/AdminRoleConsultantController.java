package studentConsulting.controller.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.payload.dto.manage.ManageRoleConsultantDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminRoleConsultantService;
import studentConsulting.service.interfaces.common.IExcelService;
import studentConsulting.service.interfaces.common.IPdfService;

import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AdminRoleConsultantController {

    @Autowired
    private IAdminRoleConsultantService roleConsultantService;

    @Autowired
    private IExcelService excelService;

    @Autowired
    private IPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/role-consultant/list")
    public ResponseEntity<DataResponse<Page<ManageRoleConsultantDTO>>> getRoleConsultants(
            @RequestParam(required = false) String name,
            @RequestParam(required = false) Integer roleId, // thêm roleId vào request
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageRoleConsultantDTO> roleConsultants = roleConsultantService.getRoleConsultantByAdmin(
                name,
                Optional.ofNullable(roleId),
                pageable);

        return ResponseEntity.ok(
                DataResponse.<Page<ManageRoleConsultantDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách role consultant thành công")
                        .data(roleConsultants)
                        .build()
        );
    }

//    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
//    @PostMapping("/admin/role-consultant/create")
//    public ResponseEntity<DataResponse<ManageRoleConsultantDTO>> createRoleConsultant(
//            @RequestBody RoleConsultantRequest roleConsultantRequest) {
//        if (roleConsultantRequest == null || roleConsultantRequest.getName().trim().isEmpty()) {
//            return ResponseEntity.status(400).body(
//                    DataResponse.<ManageRoleConsultantDTO>builder()
//                            .status("error")
//                            .message("Dữ liệu role consultant không hợp lệ")
//                            .build()
//            );
//        }
//
//        ManageRoleConsultantDTO savedRoleConsultant = roleConsultantService.createRoleConsultant(roleConsultantRequest);
//
//        return ResponseEntity.ok(
//                DataResponse.<ManageRoleConsultantDTO>builder()
//                        .status("success")
//                        .message("Tạo role consultant thành công")
//                        .data(savedRoleConsultant)
//                        .build()
//        );
//    }
//
//    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
//    @PutMapping("/admin/role-consultant/update")
//    public ResponseEntity<DataResponse<ManageRoleConsultantDTO>> updateRoleConsultant(
//            @RequestParam Integer id,
//            @RequestBody RoleConsultantRequest roleConsultantRequest) {
//        if (roleConsultantRequest == null || roleConsultantRequest.getName().trim().isEmpty()) {
//            return ResponseEntity.status(400).body(
//                    DataResponse.<ManageRoleConsultantDTO>builder()
//                            .status("error")
//                            .message("Dữ liệu role consultant không hợp lệ")
//                            .build()
//            );
//        }
//
//        ManageRoleConsultantDTO updatedRoleConsultant = roleConsultantService.updateRoleConsultant(id, roleConsultantRequest);
//
//        return ResponseEntity.ok(
//                DataResponse.<ManageRoleConsultantDTO>builder()
//                        .status("success")
//                        .message("Cập nhật role consultant thành công")
//                        .data(updatedRoleConsultant)
//                        .build()
//        );
//    }
//
//    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
//    @DeleteMapping("/admin/role-consultant/delete")
//    public ResponseEntity<DataResponse<Void>> deleteRoleConsultant(@RequestParam Integer id) {
//
//        roleConsultantService.deleteRoleConsultantById(id);
//        return ResponseEntity.ok(
//                DataResponse.<Void>builder()
//                        .status("success")
//                        .message("Xóa role consultant thành công")
//                        .build()
//        );
//
//    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/role-consultant/detail")
    public ResponseEntity<DataResponse<ManageRoleConsultantDTO>> getRoleConsultantById(@RequestParam Integer id) {
            ManageRoleConsultantDTO manageRoleConsultantDTO = roleConsultantService.getRoleConsultantById(id);
            return ResponseEntity.ok(
                    DataResponse.<ManageRoleConsultantDTO>builder()
                            .status("success")
                            .data(manageRoleConsultantDTO)
                            .build()
            );

    }
}
