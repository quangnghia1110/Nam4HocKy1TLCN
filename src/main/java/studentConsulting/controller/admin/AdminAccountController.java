package studentConsulting.controller.admin;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.payload.dto.authentication.ManageAccountDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.service.interfaces.admin.IAdminAccountService;
import studentConsulting.service.interfaces.common.IExcelService;
import studentConsulting.service.interfaces.common.IPdfService;

import java.time.LocalDate;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AdminAccountController {

    @Autowired
    private IAdminAccountService accountService;

    @Autowired
    private IExcelService excelService;

    @Autowired
    private IPdfService pdfService;

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/account/list")
    public ResponseEntity<DataResponse<Page<ManageAccountDTO>>> getAccounts(
            @RequestParam(required = false) String email,
            @RequestParam(required = false) String username,
            @RequestParam(required = false) Boolean isOnline,
            @RequestParam(required = false) Boolean isActivity,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createdAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageAccountDTO> accounts = accountService.getAllAccountsWithFilters(
                email,
                username,
                isOnline,
                Optional.ofNullable(startDate),
                Optional.ofNullable(endDate),
                isActivity,
                pageable);

        if (accounts.isEmpty()) {
            return ResponseEntity.status(HttpStatus.NOT_FOUND).body(
                    DataResponse.<Page<ManageAccountDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy tài khoản phù hợp.")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<ManageAccountDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách tài khoản thành công")
                        .data(accounts)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/admin/account/detail")
    public ResponseEntity<DataResponse<ManageAccountDTO>> getAccountById(@RequestParam Integer id) {
        try {
            ManageAccountDTO account = accountService.getAccountById(id);
            return ResponseEntity.ok(
                    DataResponse.<ManageAccountDTO>builder()
                            .status("success")
                            .message("Lấy thông tin tài khoản thành công")
                            .data(account)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<ManageAccountDTO>builder()
                            .status("error")
                            .message("Không tìm thấy tài khoản với ID: " + id)
                            .build()
            );
        }
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/account/change-activity")
    public ResponseEntity<DataResponse<ManageAccountDTO>> changeAccountActivity(@RequestParam Integer id) {
        try {
            ManageAccountDTO updatedAccount = accountService.changeAccountActivity(id);
            return ResponseEntity.ok(
                    DataResponse.<ManageAccountDTO>builder()
                            .status("success")
                            .message("Cập nhật trạng thái tài khoản thành công")
                            .data(updatedAccount)
                            .build()
            );
        } catch (Exception e) {
            return ResponseEntity.status(404).body(
                    DataResponse.<ManageAccountDTO>builder()
                            .status("error")
                            .message("Không tìm thấy tài khoản với ID: " + id)
                            .build()
            );
        }
    }
}
