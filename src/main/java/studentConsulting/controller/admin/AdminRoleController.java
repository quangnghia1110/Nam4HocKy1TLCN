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
import studentConsulting.model.payload.dto.actor.RoleDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminRoleService;
import studentConsulting.service.interfaces.common.IExcelService;
import studentConsulting.service.interfaces.common.IPdfService;

@RestController
@RequestMapping("${base.url}")
public class AdminRoleController {

    @Autowired
    private IAdminRoleService roleService;

    @Autowired
    private IExcelService excelService;

    @Autowired
    private IPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/role/list")
    public ResponseEntity<DataResponse<Page<RoleDTO>>> getRoles(
            @RequestParam(required = false) String name,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "id") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<RoleDTO> roles = roleService.getRoleByAdmin(name, pageable);

        return ResponseEntity.ok(
                DataResponse.<Page<RoleDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách vai trò thành công")
                        .data(roles)
                        .build()
        );
    }

//    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
//    @PostMapping("/admin/role/create")
//    public ResponseEntity<DataResponse<RoleDTO>> createRole(@RequestBody RoleRequest roleRequest) {
//        if (roleRequest == null || roleRequest.getName().trim().isEmpty()) {
//            return ResponseEntity.status(400).body(
//                    DataResponse.<RoleDTO>builder()
//                            .status("error")
//                            .message("Dữ liệu vai trò không hợp lệ")
//                            .build()
//            );
//        }
//
//        RoleDTO savedRole = roleService.createRole(roleRequest);
//
//        return ResponseEntity.ok(
//                DataResponse.<RoleDTO>builder()
//                        .status("success")
//                        .message("Tạo vai trò thành công")
//                        .data(savedRole)
//                        .build()
//        );
//    }
//
//    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
//    @PutMapping("/admin/role/update")
//    public ResponseEntity<DataResponse<RoleDTO>> updateRole(@RequestParam Integer id, @RequestBody RoleRequest roleRequest) {
//        if (roleRequest == null || roleRequest.getName().trim().isEmpty()) {
//            return ResponseEntity.status(400).body(
//                    DataResponse.<RoleDTO>builder()
//                            .status("error")
//                            .message("Dữ liệu vai trò không hợp lệ")
//                            .build()
//            );
//        }
//
//        RoleDTO updatedRole = roleService.updateRole(id, roleRequest);
//
//        return ResponseEntity.ok(
//                DataResponse.<RoleDTO>builder()
//                        .status("success")
//                        .message("Cập nhật vai trò thành công")
//                        .data(updatedRole)
//                        .build()
//        );
//    }
//
//    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
//    @DeleteMapping("/admin/role/delete")
//    public ResponseEntity<DataResponse<Void>> deleteRole(@RequestParam Integer id) {
//        try {
//            roleService.deleteRoleById(id);
//            return ResponseEntity.ok(
//                    DataResponse.<Void>builder()
//                            .status("success")
//                            .message("Xóa vai trò thành công")
//                            .build()
//            );
//        } catch (Exception e) {
//            return ResponseEntity.status(404).body(
//                    DataResponse.<Void>builder()
//                            .status("error")
//                            .message("Không tìm thấy vai trò để xóa")
//                            .build()
//            );
//        }
//    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/role/detail")
    public ResponseEntity<DataResponse<RoleDTO>> getRoleById(@RequestParam Integer id) {
            RoleDTO RoleDTO = roleService.getRoleById(id);
            return ResponseEntity.ok(
                    DataResponse.<RoleDTO>builder()
                            .status("success")
                            .data(RoleDTO)
                            .build()
            );

    }
}
