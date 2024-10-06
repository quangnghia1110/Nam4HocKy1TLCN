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
import studentConsulting.model.payload.dto.consultation_schedule.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.request.consultant.ManageCreateConsultantScheduleRequest;
import studentConsulting.model.payload.request.consultant.UpdateConsultationScheduleRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.consultation_schedule.ConsultationScheduleRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorConsultationScheduleService;
import studentConsulting.service.interfaces.common.ICommonNotificationService;
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

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor/consultation-schedule/list")
    public ResponseEntity<DataResponse<Page<ConsultationScheduleDTO>>> getConsultationSchedulesByDepartment(
            @RequestParam(required = false) String title,
            @RequestParam(required = false) Boolean statusPublic,
            @RequestParam(required = false) Boolean statusConfirmed,
            @RequestParam(required = false) Boolean mode,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "title") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals("ROLE_ADMIN");
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ConsultationScheduleDTO> schedules = isAdmin
                ? consultationScheduleService.getAllConsultationSchedulesWithFilters(title, statusPublic, statusConfirmed, mode, startDate, endDate, pageable)
                : consultationScheduleService.getConsultationsByDepartmentWithFilters(
                user.getAccount().getDepartment().getId(), title, statusPublic, statusConfirmed, mode, startDate, endDate, pageable);

        if (schedules.isEmpty()) {
            return ResponseEntity.status(404).body(DataResponse.<Page<ConsultationScheduleDTO>>builder()
                    .status("error")
                    .message("Không tìm thấy lịch tư vấn.")
                    .build());
        }

        return ResponseEntity.ok(DataResponse.<Page<ConsultationScheduleDTO>>builder()
                .status("success")
                .message("Lấy danh sách lịch tư vấn thành công.")
                .data(schedules)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor/consultation-schedule/create")
    public ResponseEntity<DataResponse<ManageConsultantScheduleDTO>> createConsultationSchedule(
            @RequestBody ManageCreateConsultantScheduleRequest request,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals("ROLE_ADMIN");

        Integer departmentId = isAdmin ? null : user.getAccount().getDepartment().getId(); // Admin có thể tạo ở bất kỳ phòng ban nào
        Integer userId = user.getId();

        ManageConsultantScheduleDTO consultationSchedule = consultationScheduleService.createConsultationSchedule(request, departmentId, userId);

        return ResponseEntity.ok(DataResponse.<ManageConsultantScheduleDTO>builder()
                .status("success")
                .message("Tạo buổi tư vấn thành công.")
                .data(consultationSchedule)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor/consultation-schedule-owner/list")
    public ResponseEntity<DataResponse<Page<ManageConsultantScheduleDTO>>> getConsultationsByDepartmentOwner(
            @RequestParam(required = false) String title,
            @RequestParam(required = false) Boolean statusPublic,
            @RequestParam(required = false) Boolean statusConfirmed,
            @RequestParam(required = false) Boolean mode,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "title") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> managerOpt = userRepository.findUserInfoByEmail(email);
        if (!managerOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = managerOpt.get();
        boolean isAdmin = manager.getAccount().getRole().getName().equals("ROLE_ADMIN");

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<ManageConsultantScheduleDTO> schedules = isAdmin
                ? consultationScheduleService.getAllConsultationsWithFilters(title, statusPublic, statusConfirmed, mode, startDate, endDate, pageable)
                : consultationScheduleService.getConsultationsByDepartmentOwnerWithFilters(
                manager.getAccount().getDepartment().getId(), title, statusPublic, statusConfirmed, mode, startDate, endDate, pageable);

        if (schedules.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<ManageConsultantScheduleDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy lịch tư vấn.")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<ManageConsultantScheduleDTO>>builder()
                        .status("success")
                        .message("Lấy danh sách lịch tư vấn thành công.")
                        .data(schedules)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping(value = "/advisor/consultation-schedule/update", consumes = {"multipart/form-data"})
    public DataResponse<ManageConsultantScheduleDTO> updateConsultationSchedule(
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
        boolean isAdmin = user.getAccount().getRole().getName().equals("ROLE_ADMIN");

        Integer departmentId = isAdmin ? null : user.getAccount().getDepartment().getId();

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

        ManageConsultantScheduleDTO updatedScheduleDTO = isAdmin
                ? consultationScheduleService.updateConsultationScheduleAsAdmin(scheduleId, scheduleRequest)
                : consultationScheduleService.updateConsultationSchedule(scheduleId, departmentId, scheduleRequest);

        return DataResponse.<ManageConsultantScheduleDTO>builder()
                .status("success")
                .message("Cập nhật lịch tư vấn thành công.")
                .data(updatedScheduleDTO)
                .build();
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/advisor/consultation-schedule/delete")
    public ResponseEntity<DataResponse<Void>> deleteConsultationSchedule(@RequestParam Integer scheduleId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals("ROLE_ADMIN");

        if (isAdmin) {
            consultationScheduleService.deleteConsultationScheduleAsAdmin(scheduleId); // Admin có quyền xóa tất cả các lịch tư vấn
        } else {
            consultationScheduleService.deleteConsultationSchedule(scheduleId, user.getAccount().getDepartment().getId());
        }

        return ResponseEntity.ok(DataResponse.<Void>builder()
                .status("success")
                .message("Xóa lịch tư vấn thành công.")
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor/consultation-schedule/list-member-join")
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
        boolean isAdmin = user.getAccount().getRole().getName().equals("ROLE_ADMIN");

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


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor/consultation-schedule/detail-consultant")
    public ResponseEntity<DataResponse<ConsultationScheduleDTO>> getConsultationScheduleById(
            @RequestParam("id") Integer scheduleId, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals("ROLE_ADMIN");

        ConsultationScheduleDTO scheduleDTO;
        if (isAdmin) {
            scheduleDTO = consultationScheduleService.getConsultationScheduleById(scheduleId);
        } else {
            scheduleDTO = consultationScheduleService.getConsultationScheduleByIdAndDepartment(scheduleId, user.getAccount().getDepartment().getId());
        }

        if (scheduleDTO == null) {
            throw new ErrorException("Không tìm thấy lịch tư vấn.");
        }

        return ResponseEntity.ok(DataResponse.<ConsultationScheduleDTO>builder()
                .status("success")
                .message("Lấy chi tiết lịch tư vấn thành công.")
                .data(scheduleDTO)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor/consultation-schedule/detail-owner")
    public ResponseEntity<DataResponse<ManageConsultantScheduleDTO>> getConsultationScheduleByIdAndCreatedBy(
            @RequestParam("id") Integer scheduleId, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals("ROLE_ADMIN");

        ManageConsultantScheduleDTO scheduleDTO;
        if (isAdmin) {
            scheduleDTO = consultationScheduleService.getConsultationScheduleByIds(scheduleId);
        } else {
            scheduleDTO = consultationScheduleService.getConsultationScheduleByIdAndCreatedBy(scheduleId, user.getId());
        }

        if (scheduleDTO == null) {
            throw new ErrorException("Không tìm thấy lịch tư vấn.");
        }

        return ResponseEntity.ok(DataResponse.<ManageConsultantScheduleDTO>builder()
                .status("success")
                .message("Lấy chi tiết lịch tư vấn thành công.")
                .data(scheduleDTO)
                .build());
    }
}
