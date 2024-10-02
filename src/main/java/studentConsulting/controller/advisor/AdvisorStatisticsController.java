package studentConsulting.controller.advisor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.statistic.AdvisorStatisticsDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorStatisticsService;

import java.security.Principal;
import java.util.List;
import java.util.Map;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AdvisorStatisticsController {
    @Autowired
    private IAdvisorStatisticsService statisticsService;

    @Autowired
    private UserRepository userRepository;

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/advisor/statistics")
    public ResponseEntity<DataResponse<AdvisorStatisticsDTO>> getManagerStatistics(Principal principal) {
        String email = principal.getName();
        System.out.println("Email: " + email);

        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        AdvisorStatisticsDTO data = statisticsService.getAdvisorStatistics(user.getId());

        if (data == null) {
            throw new ErrorException("Không tìm thấy thống kê của trưởng ban");
        }

        return ResponseEntity.ok(DataResponse.<AdvisorStatisticsDTO>builder()
                .status("success")
                .message("Lấy thống kê của trưởng ban thành công")
                .data(data)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/advisor/statistics/questions-deleted/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getDeletedQuestionsByYear(
            Principal principal,
            @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer departmentId = user.getAccount().getDepartment().getId();
        List<Map<String, Object>> data = statisticsService.getDeletedQuestionsByYear(departmentId, year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy danh sách câu hỏi bị xóa thành công.")
                .data(data)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/advisor/statistics/answers-given/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getAnswersGivenByYear(
            Principal principal,
            @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer departmentId = user.getAccount().getDepartment().getId();
        List<Map<String, Object>> data = statisticsService.getAnswersGivenByYear(departmentId, year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy danh sách câu trả lời thành công.")
                .data(data)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/advisor/statistics/answer-approval/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getAnswerApprovalByYear(
            Principal principal,
            @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer departmentId = user.getAccount().getDepartment().getId();
        List<Map<String, Object>> data = statisticsService.getAnswerApprovalByYear(departmentId, year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy danh sách câu trả lời được duyệt thành công.")
                .data(data)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/advisor/statistics/consultation-schedules/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getConsultationSchedulesByYear(
            Principal principal,
            @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer departmentId = user.getAccount().getDepartment().getId();
        List<Map<String, Object>> data = statisticsService.getConsultationSchedulesConsultantByYear(departmentId, year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy danh sách lịch tư vấn thành công.")
                .data(data)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/advisor/statistics/conversations/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getConversationsByYear(
            Principal principal,
            @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer departmentId = user.getAccount().getDepartment().getId();
        List<Map<String, Object>> data = statisticsService.getConversationsConsultantByYear(departmentId, year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy danh sách cuộc trò chuyện thành công.")
                .data(data)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/advisor/statistics/ratings/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getRatingsByYear(
            Principal principal,
            @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer departmentId = user.getAccount().getDepartment().getId();
        List<Map<String, Object>> data = statisticsService.getRatingsByYear(departmentId, year);
        if (data.isEmpty()) {
            throw new ErrorException("Không tìm thấy dữ liệu");
        }
        return ResponseEntity.ok(DataResponse.<List<Map<String, Object>>>builder()
                .status("success")
                .message("Lấy danh sách đánh giá thành công.")
                .data(data)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/advisor/statistics/common-questions/yearly")
    public ResponseEntity<DataResponse<List<Map<String, Object>>>> getCommonQuestionsByYear(
            Principal principal,
            @RequestParam int year) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        Integer departmentId = user.getAccount().getDepartment().getId();
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
}
