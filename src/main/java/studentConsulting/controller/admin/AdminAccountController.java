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
import studentConsulting.model.entity.AccountEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.Exceptions;
import studentConsulting.model.payload.dto.manage.ManageAccountDTO;
import studentConsulting.model.payload.dto.manage.ManageActivityDTO;
import studentConsulting.model.payload.dto.manage.UpdateAccountDTO;
import studentConsulting.model.payload.dto.manage.UpdateActivityDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.interfaces.admin.IAdminAccountService;
import studentConsulting.service.interfaces.common.IExcelService;
import studentConsulting.service.interfaces.common.IPdfService;

import java.security.Principal;
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

    @Autowired
    private UserRepository userRepository;

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

        Page<ManageAccountDTO> accounts = accountService.getAccountByAdmin(
                email,
                username,
                isOnline,
                Optional.ofNullable(startDate),
                Optional.ofNullable(endDate),
                isActivity,
                pageable);

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
            ManageAccountDTO account = accountService.getAccountById(id);
            return ResponseEntity.ok(
                    DataResponse.<ManageAccountDTO>builder()
                            .status("success")
                            .data(account)
                            .build()
            );

    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping("/admin/account/update")
    public ResponseEntity<DataResponse<ManageAccountDTO>> updateAccount(
            @RequestParam Integer id,
            @RequestBody UpdateAccountDTO accountRequest) {

        ManageAccountDTO updatedAccount = accountService.updateAccount(id, accountRequest);

        return ResponseEntity.ok(
                DataResponse.<ManageAccountDTO>builder()
                        .status("success")
                        .message("Cập nhật tài khoản thành công")
                        .data(updatedAccount)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @PutMapping("/activity/update")
    public ResponseEntity<DataResponse<ManageActivityDTO>> updateActivity(
            @RequestParam Integer id,
            @RequestBody UpdateActivityDTO accountRequest,
            Principal principal) {

        String email = principal.getName();

        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new Exceptions.ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        boolean isConsultantOrManager = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TUVANVIEN)
                || user.getAccount().getRole().getName().equals(SecurityConstants.Role.TRUONGBANTUVAN);

        if (isAdmin || isConsultantOrManager) {
            ManageActivityDTO updatedActivity = accountService.updateActivity(id, accountRequest);

            return ResponseEntity.ok(
                    DataResponse.<ManageActivityDTO>builder()
                            .status("success")
                            .message("Cập nhật trạng thái hoạt động thành công")
                            .data(updatedActivity)
                            .build()
            );
        } else {
            throw new Exceptions.ErrorException("Bạn không có quyền thay đổi trạng thái hoạt động.");
        }
    }

}
