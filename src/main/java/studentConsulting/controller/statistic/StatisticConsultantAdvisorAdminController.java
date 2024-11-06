package studentConsulting.controller.statistic;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.interfaces.statistic.IStatisticConsultantAdvisorAdminService;

import java.security.Principal;
import java.time.LocalDate;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class StatisticConsultantAdvisorAdminController {
    @Autowired
    private IStatisticConsultantAdvisorAdminService statisticsService;

    @Autowired
    private UserRepository userRepository;

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/statistics")
    public ResponseEntity<DataResponse<?>> getStatistics(
            Principal principal,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        String role = user.getAccount().getRole().getName();
        Integer userId = user.getId();

        Object data = statisticsService.getStatisticsByRole(userId, role, startDate, endDate);

        if (data == null) {
            throw new ErrorException("Không tìm thấy dữ liệu thống kê");
        }

        return ResponseEntity.ok(DataResponse.builder()
                .status("success")
                .message("Lấy thống kê thành công")
                .data(data)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN + " or " + SecurityConstants.PreAuthorize.TUVANVIEN)
    @GetMapping("/statistics/questions-deleted/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getDeletedQuestionsByYear(
            Principal principal,
            @RequestParam int year) {

        if (principal == null) {
            throw new ErrorException("Người dùng chưa được xác thực");
        }

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        UserInformationEntity user = userOpt.orElseThrow(() -> new ErrorException("Không tìm thấy người dùng"));

        Integer departmentId = null;
        Integer consultantId = null;

        switch (user.getAccount().getRole().getName()) {
            case SecurityConstants.Role.ADMIN:
                break;
            case SecurityConstants.Role.TRUONGBANTUVAN:
                departmentId = user.getAccount().getDepartment().getId();
                break;
            case SecurityConstants.Role.TUVANVIEN:
                consultantId = user.getId();
                break;
            default:
                throw new ErrorException("Quyền truy cập không hợp lệ");
        }

        List<Map<String, Object>> data = statisticsService.getDeletedQuestionsByYear(departmentId, year, consultantId);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }

        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy danh sách câu hỏi bị xóa thành công.")
                .data(data)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/statistics/questions-forwarded/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getForwardQuestionsByYear(
            Principal principal, @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer consultantId = null;
        Integer departmentId = null;
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        boolean isAdvisor = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TRUONGBANTUVAN);

        if (isAdmin) {
            departmentId = null;
        } else if (isAdvisor) {
            departmentId = user.getAccount().getDepartment().getId();
        } else {
            consultantId = user.getId();
        }

        List<Map<String, Object>> data = statisticsService.getForwardQuestionsByYear(consultantId, departmentId, year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }

        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy thống kê câu hỏi chuyển tiếp thành công.")
                .data(data)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/statistics/answers-given/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getAnswersGivenByYear(
            Principal principal, @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer consultantId = null;
        Integer departmentId = null;
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        boolean isAdvisor = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TRUONGBANTUVAN);

        if (isAdmin) {
            departmentId = null;
        } else if (isAdvisor) {
            departmentId = user.getAccount().getDepartment().getId();
        } else {
            consultantId = user.getId();
        }

        List<Map<String, Object>> data = statisticsService.getAnswersGivenByRole(consultantId, departmentId, year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }

        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy thống kê câu trả lời đã đưa ra thành công.")
                .data(data)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/statistics/answer-approval/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getAnswerApprovalByYear(
            Principal principal, @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer consultantId = null;
        Integer departmentId = null;
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        boolean isAdvisor = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TRUONGBANTUVAN);

        if (isAdmin) {
            departmentId = null;
        } else if (isAdvisor) {
            departmentId = user.getAccount().getDepartment().getId();
        } else {
            consultantId = user.getId();
        }

        List<Map<String, Object>> data = statisticsService.getAnswerApprovalByRole(consultantId, departmentId, year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }

        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy danh sách câu trả lời được duyệt thành công.")
                .data(data)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/statistics/consultation-schedules/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getConsultationSchedulesByYear(
            Principal principal, @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer consultantId = null;
        Integer departmentId = null;
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        boolean isAdvisor = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TRUONGBANTUVAN);

        if (isAdmin) {
            departmentId = null;
        } else if (isAdvisor) {
            departmentId = user.getAccount().getDepartment().getId();
        } else {
            consultantId = user.getId();
        }

        List<Map<String, Object>> data = statisticsService.getConsultationSchedulesByRole(consultantId, departmentId, year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }

        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy thống kê lịch tư vấn thành công.")
                .data(data)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/statistics/conversations/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getConversationsByYear(
            Principal principal, @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer consultantId = null;
        Integer departmentId = null;
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        boolean isAdvisor = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TRUONGBANTUVAN);

        if (isAdmin) {
            departmentId = null;
        } else if (isAdvisor) {
            departmentId = user.getAccount().getDepartment().getId();
        } else {
            consultantId = user.getId();
        }

        List<Map<String, Object>> data = statisticsService.getConversationsByRole(consultantId, departmentId, year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }

        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy thống kê cuộc hội thoại thành công.")
                .data(data)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/statistics/ratings/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getRatingsByYear(
            Principal principal, @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer consultantId = null;
        Integer departmentId = null;
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        boolean isAdvisor = user.getAccount().getRole().getName().equals(SecurityConstants.Role.TRUONGBANTUVAN);

        if (isAdmin) {
            departmentId = null;
        } else if (isAdvisor) {
            departmentId = user.getAccount().getDepartment().getId();
        } else {
            consultantId = user.getId();
        }

        List<Map<String, Object>> data = statisticsService.getRatingsByRole(consultantId, departmentId, year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }

        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy danh sách đánh giá thành công.")
                .data(data)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/statistics/posts/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getApprovedPostsByYear(
            Principal principal, @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer consultantId = null;
        Integer departmentId = null;
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        if (isAdmin) {
            departmentId = null;
        } else {
            consultantId = user.getId();
        }

        List<Map<String, Object>> data = statisticsService.getApprovedAndPendingPostsByYear(consultantId, departmentId, year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }

        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy thống kê bài đăng đã duyệt và chưa duyệt thành công.")
                .data(data)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/statistics/common-questions/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getCommonQuestionsByYear(
            Principal principal, @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        Integer departmentId = isAdmin ? null : user.getAccount().getDepartment().getId();
        List<Map<String, Object>> data = statisticsService.getCommonQuestionsByYear(departmentId, year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy danh sách câu hỏi chung thành công.")
                .data(data)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TUVANVIEN + " or " + SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/statistics/consultingByMessage/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getUniqueUsersAdvisedByRole(
            Principal principal, @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        String role = user.getAccount().getRole().getName();
        Integer consultantId = null;
        Integer departmentId = null;

        if (role.equals(SecurityConstants.Role.ADMIN)) {
        } else if (role.equals(SecurityConstants.Role.TRUONGBANTUVAN)) {
            departmentId = user.getAccount().getDepartment().getId();
        } else if (role.equals(SecurityConstants.Role.TUVANVIEN)) {
            consultantId = user.getId();
        } else {
            throw new ErrorException("Quyền không hợp lệ");
        }

        List<Map<String, Object>> monthlyUserAdviceCount = statisticsService.getUniqueUsersAdvisedByMessages(consultantId, departmentId, year);

        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy số lượng người dùng đã được tư vấn qua tin nhắn thành công.")
                .data(monthlyUserAdviceCount)
                .build());
    }
}
