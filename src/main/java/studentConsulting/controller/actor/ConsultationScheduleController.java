package studentConsulting.controller.actor;

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
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.ConsultationScheduleEntity;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.ConsultationScheduleDTO;
import studentConsulting.model.payload.dto.actor.ConsultationScheduleRegistrationDTO;
import studentConsulting.model.payload.dto.actor.ConsultationScheduleRegistrationMemberDTO;
import studentConsulting.model.payload.dto.actor.PostDTO;
import studentConsulting.model.payload.dto.manage.ManageConsultantScheduleDTO;
import studentConsulting.model.payload.request.ConsultationScheduleRegistrationRequest;
import studentConsulting.model.payload.request.CreateScheduleConsultationRequest;
import studentConsulting.model.payload.request.ManageCreateConsultantScheduleRequest;
import studentConsulting.model.payload.request.UpdateConsultationScheduleRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.actor.ConsultationScheduleRepository;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.interfaces.actor.IConsultationScheduleService;
import studentConsulting.service.interfaces.common.INotificationService;
import studentConsulting.service.interfaces.common.IUserService;

import java.security.Principal;
import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class ConsultationScheduleController {

    @Autowired
    private IConsultationScheduleService consultationScheduleService;

    @Autowired
    private IUserService userService;

    @Autowired
    private INotificationService notificationService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ConsultationScheduleRepository consultationScheduleRepository;

    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @PostMapping("/user/consultation-schedule/create")
    public ResponseEntity<DataResponse<ConsultationScheduleDTO>> createConsultation(
            @RequestBody CreateScheduleConsultationRequest request, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity user = userOpt.get();

        ConsultationScheduleDTO createdSchedule = consultationScheduleService.createConsultation(request, user);

        UserInformationEntity consultant = userService.findConsultantById(request.getConsultantId())
                .orElseThrow(() -> new ErrorException("Tư vấn viên không tồn tại"));

        notificationService.sendUserNotification(
                user.getId(),
                consultant.getId(),
                NotificationContent.NEW_CONSULTATION_SCHEDULE.formatMessage(user.getLastName() + " " + user.getFirstName()),
                NotificationType.TUVANVIEN
        );

        return ResponseEntity.ok(DataResponse.<ConsultationScheduleDTO>builder().status("success")
                .message("Lịch tư vấn đã được tạo thành công.").data(createdSchedule).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN)
    @PostMapping(value = "/consultant/consultation-schedule/confirm", consumes = {"application/json"})
    public DataResponse<ManageConsultantScheduleDTO> confirmConsultationScheduleForConsultant(
            @RequestParam("scheduleId") Integer scheduleId,
            @RequestBody UpdateConsultationScheduleRequest scheduleRequest,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        if (!user.getAccount().getRoleConsultant().getName().equals(SecurityConstants.RoleConsultant.GIANGVIEN)) {
            throw new ErrorException("Chỉ có giảng viên mới có thể xác nhận lịch tư vấn.");
        }

        ConsultationScheduleEntity schedule = consultationScheduleRepository.findById(scheduleId)
                .orElseThrow(() -> new ErrorException("Lịch tư vấn không tồn tại"));

        if (schedule.getStatusConfirmed() != null && schedule.getStatusConfirmed()) {
            throw new ErrorException("Bạn không thể cập nhật lịch tư vấn này vì nó đã được xác nhận.");
        }

        if (!schedule.getConsultant().equals(user) || !schedule.getDepartment().getId().equals(user.getAccount().getDepartment().getId())) {
            throw new ErrorException("Bạn không có quyền cập nhật lịch tư vấn này");
        }

        ManageConsultantScheduleDTO updatedScheduleDTO = consultationScheduleService.confirmConsultationSchedule(
                scheduleId, user.getAccount().getDepartment().getId(), scheduleRequest);

        Optional<UserInformationEntity> advisorOpt = userRepository.findByRoleAndDepartment(
                SecurityConstants.Role.TRUONGBANTUVAN, user.getAccount().getDepartment().getId());

        advisorOpt.ifPresent(headOfDepartment -> {
            notificationService.sendUserNotification(
                    user.getId(),
                    headOfDepartment.getId(),
                    NotificationContent.CONSULTATION_SCHEDULE_CONFIRMED.formatMessage(user.getLastName() + " " + user.getFirstName()),
                    NotificationType.TRUONGBANTUVAN
            );
        });

        UserInformationEntity registeredUser = schedule.getUser();
        notificationService.sendUserNotification(
                user.getId(),
                registeredUser.getId(),
                NotificationContent.CONSULTATION_SCHEDULE_CONFIRMED.formatMessage(user.getLastName() + " " + user.getFirstName()),
                NotificationType.USER
        );


        return DataResponse.<ManageConsultantScheduleDTO>builder()
                .status("success")
                .message("Xác nhận lịch tư vấn thành công.")
                .data(updatedScheduleDTO)
                .build();
    }

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

        Integer departmentId = isAdmin ? null : (user.getAccount().getDepartment() != null ? user.getAccount().getDepartment().getId() : null);

        if (!isAdmin && departmentId == null) {
            throw new ErrorException("Người dùng không thuộc phòng ban nào.");
        }
        Integer userId = user.getId();
        ConsultationScheduleDTO consultationSchedule = consultationScheduleService.createConsultationSchedule(request, departmentId, userId);
        List<UserInformationEntity> users = userRepository.findAll();
        for (UserInformationEntity targetUser : users) {
            notificationService.sendUserNotification(
                    user.getId(),
                    targetUser.getId(),
                    NotificationContent.NEW_CONSULTATION_SCHEDULE.formatMessage(user.getLastName() + " " + user.getFirstName()),
                    NotificationType.USER
            );
        }

        if (!isAdmin) {
            List<UserInformationEntity> admins = userRepository.findAllByRole(SecurityConstants.Role.ADMIN); // Lấy tất cả admin
            for (UserInformationEntity admin : admins) {
                notificationService.sendUserNotification(
                        user.getId(),
                        admin.getId(),
                        NotificationContent.NEW_CONSULTATION_SCHEDULE_ADMIN.formatMessage(user.getLastName() + " " + user.getFirstName()),
                        NotificationType.ADMIN
                );
            }
        }
        return ResponseEntity.ok(DataResponse.<ConsultationScheduleDTO>builder()
                .status("success")
                .message("Tạo buổi tư vấn thành công.")
                .data(consultationSchedule)
                .build());
    }

    @GetMapping("/consultation-schedule/list")
    public ResponseEntity<DataResponse<Page<ConsultationScheduleDTO>>> getConsultationSchedulesByRole(
            @RequestParam(required = false) String title,
            @RequestParam(required = false) Boolean type,
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

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConsultationScheduleDTO> schedules;

        if (principal == null) {
            schedules = consultationScheduleService.getConsultationScheduleForGuest(pageable);
            return ResponseEntity.ok(DataResponse.<Page<ConsultationScheduleDTO>>builder()
                    .status("success")
                    .message("Lấy danh sách lịch tư vấn thành công.")
                    .data(schedules)
                    .build());
        }

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        // Xử lý lấy danh sách lịch tư vấn theo vai trò người dùng
        schedules = consultationScheduleService.getConsultationScheduleByRole(user, title, type, statusPublic, statusConfirmed, mode, startDate, endDate, pageable);

        return ResponseEntity.ok(DataResponse.<Page<ConsultationScheduleDTO>>builder()
                .status("success")
                .message("Lấy danh sách lịch tư vấn thành công.")
                .data(schedules)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.USER + " or " + SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping(value = "/consultation-schedule/update", consumes = {"application/json"})
    public DataResponse<ConsultationScheduleDTO> updateConsultationSchedule(
            @RequestBody UpdateConsultationScheduleRequest scheduleRequest,
            @RequestParam Integer scheduleId,
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

        System.out.println(userId + " _ " + departmentId);
        ConsultationScheduleDTO scheduleDTO = consultationScheduleService.getDetailConsultationScheduleByRole(scheduleId, role, departmentId, userId);

        return ResponseEntity.ok(DataResponse.<ConsultationScheduleDTO>builder()
                .status("success")
                .data(scheduleDTO)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @GetMapping("/user/consultation-schedule/list-join")
    public ResponseEntity<DataResponse<Page<ConsultationScheduleRegistrationDTO>>> getSchedulesJoinByUser(
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

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<ConsultationScheduleRegistrationDTO> schedules = consultationScheduleService.getSchedulesJoinByUser(
                user, startDate, endDate, pageable);

        return ResponseEntity.ok(DataResponse.<Page<ConsultationScheduleRegistrationDTO>>builder()
                .status("success").message("Lấy danh sách lịch tư vấn thành công.").data(schedules).build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @PostMapping("/user/consultation-schedule/join")
    public ResponseEntity<DataResponse<ConsultationScheduleRegistrationDTO>> joinConsultationSchedule(
            @RequestParam Integer scheduleId, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);

        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        UserInformationEntity user = userOpt.get();

        ConsultationScheduleEntity consultationSchedule = consultationScheduleRepository.findById(scheduleId)
                .orElseThrow(() -> new ErrorException("Lịch tư vấn không tồn tại"));

        ConsultationScheduleRegistrationDTO registrationDTO = consultationScheduleService.registerForConsultation(scheduleId, user);

        Optional<UserInformationEntity> advisorOpt = userRepository.findByRoleAndDepartment(
                SecurityConstants.Role.TRUONGBANTUVAN, consultationSchedule.getDepartment().getId());

        advisorOpt.ifPresent(headOfDepartment -> {
            notificationService.sendUserNotification(
                    user.getId(),
                    headOfDepartment.getId(),
                    NotificationContent.NEW_CONSULTATION_PARTICIPANT.formatMessage(user.getLastName() + " " + user.getFirstName()),
                    NotificationType.TRUONGBANTUVAN
            );
        });

        return ResponseEntity.ok(DataResponse.<ConsultationScheduleRegistrationDTO>builder()
                .status("success")
                .message("Đăng ký lịch tư vấn công khai thành công.")
                .data(registrationDTO)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @PostMapping("/user/consultation-schedule/cancel")
    public ResponseEntity<DataResponse<Void>> cancelConsultationSchedule(
            @RequestParam Integer scheduleId, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        consultationScheduleService.cancelRegistrationForConsultation(scheduleId, user);

        return ResponseEntity.ok(DataResponse.<Void>builder()
                .status("success")
                .message("Hủy đăng ký lịch tư vấn thành công.")
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.USER)
    @PostMapping("/user/consultation-schedule/check")
    public ResponseEntity<DataResponse<Boolean>> checkParticipation(
            @RequestParam Integer scheduleId, Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        ConsultationScheduleEntity schedule = consultationScheduleRepository.findById(scheduleId)
                .orElseThrow(() -> new ErrorException("Lịch tư vấn không tồn tại"));

        boolean isRegistered = consultationScheduleRepository.existsByUserAndConsultationSchedule(user, schedule);

        return ResponseEntity.ok(DataResponse.<Boolean>builder()
                .status("success")
                .message("Kiểm tra tham gia thành công.")
                .data(isRegistered)
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

        return ResponseEntity.ok(DataResponse.<Page<ConsultationScheduleRegistrationMemberDTO>>builder()
                .status("success").message("Lấy danh sách thành viên thành công.").data(members).build());
    }
}
