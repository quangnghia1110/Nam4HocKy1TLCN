package studentConsulting.controller.advisor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.consultation_schedule.ConsultationScheduleRegistrationMemberDTO;
import studentConsulting.model.payload.request.consultant.ManageCreateConsultantScheduleRequest;
import studentConsulting.model.payload.request.consultant.UpdateConsultationScheduleRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.consultation_schedule.ConsultationScheduleRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorConsultationScheduleService;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonNotificationService;
import studentConsulting.service.interfaces.common.ICommonPdfService;
import studentConsulting.service.interfaces.common.ICommonUserService;

import java.security.Principal;
import java.time.LocalDate;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AdvisorConsultationScheduleController {

    @Autowired
    private IAdvisorConsultationScheduleService consultationScheduleService;

    @Autowired
    private ICommonUserService userService;

    @Autowired
    private ICommonNotificationService notificationService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ConsultationScheduleRepository consultationScheduleRepository;

    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/consultation-schedule/create")
    public ResponseEntity<DataResponse<ConsultationScheduleDTO>> createConsultationSchedule(
            @RequestBody ManageCreateConsultantScheduleRequest request,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        Integer departmentId = isAdmin ? null : user.getAccount().getDepartment().getId();
        Integer userId = user.getId();

        ConsultationScheduleDTO consultationSchedule = consultationScheduleService.createConsultationSchedule(request, departmentId, userId);

        return ResponseEntity.ok(DataResponse.<ConsultationScheduleDTO>builder()
                .status("success")
                .message("Tạo buổi tư vấn thành công.")
                .data(consultationSchedule)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping(value = "/consultation-schedule/update", consumes = {"multipart/form-data"})
    public DataResponse<ConsultationScheduleDTO> updateConsultationSchedule(
            @RequestParam("scheduleId") Integer scheduleId,
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestParam("consultationDate") @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate consultationDate,
            @RequestParam("consultationTime") String consultationTime,
            @RequestParam("location") String location,
            @RequestParam("link") String link,
            @RequestParam("mode") Boolean mode,
            @RequestParam("statusPublic") Boolean statusPublic,
            @RequestParam("statusConfirmed") Boolean statusConfirmed,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        String role = user.getAccount().getRole().getName();
        boolean isAdmin = SecurityConstants.Role.ADMIN.equals(role);
        Integer departmentId = isAdmin ? null : user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;

        UpdateConsultationScheduleRequest scheduleRequest = UpdateConsultationScheduleRequest.builder()
                .title(title)
                .content(content)
                .consultationDate(consultationDate)
                .consultationTime(consultationTime)
                .location(location)
                .link(link)
                .mode(mode)
                .statusPublic(statusPublic)
                .statusConfirmed(statusConfirmed)
                .build();

        ConsultationScheduleDTO updatedScheduleDTO = consultationScheduleService.updateConsultationSchedule(scheduleId, departmentId, isAdmin, scheduleRequest, role, user.getId());

        return DataResponse.<ConsultationScheduleDTO>builder()
                .status("success")
                .message("Cập nhật lịch tư vấn thành công.")
                .data(updatedScheduleDTO)
                .build();
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/consultation-schedule/delete")
    public ResponseEntity<DataResponse<Void>> deleteConsultationSchedule(
            @RequestParam Integer scheduleId, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        String role = user.getAccount().getRole().getName();
        Integer departmentId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;

        consultationScheduleService.deleteConsultationSchedule(scheduleId, departmentId, user.getId(), role);

        return ResponseEntity.ok(DataResponse.<Void>builder()
                .status("success")
                .message("Xóa lịch tư vấn thành công.")
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor-admin/consultation-schedule/list-member-join")
    public ResponseEntity<DataResponse<Page<ConsultationScheduleRegistrationMemberDTO>>> getMembersByConsultationSchedule(
            @RequestParam Integer consultationScheduleId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "registeredAt") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConsultationScheduleRegistrationMemberDTO> members;

        if (isAdmin) {
            members = consultationScheduleService.getMembersByConsultationSchedule(consultationScheduleId, startDate, endDate, pageable, null);
        } else {
            members = consultationScheduleService.getMembersByConsultationSchedule(consultationScheduleId, startDate, endDate, pageable, user.getId());
        }

        if (members.isEmpty()) {
            return ResponseEntity.status(404).body(DataResponse.<Page<ConsultationScheduleRegistrationMemberDTO>>builder()
                    .status("error").message("Không tìm thấy thành viên nào tham gia buổi tư vấn này.").build());
        }

        return ResponseEntity.ok(DataResponse.<Page<ConsultationScheduleRegistrationMemberDTO>>builder()
                .status("success").message("Lấy danh sách thành viên thành công.").data(members).build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/consultation-schedule/detail")
    public ResponseEntity<DataResponse<ConsultationScheduleDTO>> getConsultationScheduleDetail(
            @RequestParam("scheduleId") Integer scheduleId, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        String role = user.getAccount().getRole().getName();
        Integer departmentId = user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null;
        Integer userId = user.getId();

        ConsultationScheduleDTO scheduleDTO = consultationScheduleService.getConsultationScheduleByRole(scheduleId, role, departmentId, userId);

        return ResponseEntity.ok(DataResponse.<ConsultationScheduleDTO>builder()
                .status("success")
                .message("Lấy chi tiết lịch tư vấn thành công.")
                .data(scheduleDTO)
                .build());
    }
}
