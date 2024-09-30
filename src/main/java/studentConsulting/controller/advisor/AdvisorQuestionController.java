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
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.QuestionFilterStatus;
import studentConsulting.model.entity.question_answer.DeletionLogEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.question_answer.MyQuestionDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.question_answer.QuestionRepository;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorQuestionService;
import studentConsulting.service.interfaces.common.ICommonNotificationService;
import studentConsulting.service.interfaces.common.ICommonUserService;

import java.security.Principal;
import java.time.LocalDate;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AdvisorQuestionController {

    @Autowired
    private IAdvisorQuestionService questionService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommonUserService userService;

    @Autowired
    private ICommonNotificationService notificationService;

    @Autowired
    private QuestionRepository questionRepository;

    @Autowired
    private SimpMessagingTemplate simpMessagingTemplate;

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/advisor/question/list-question-by-department")
    public DataResponse<Page<MyQuestionDTO>> getDepartmentConsultantsQuestionsFilters(Principal principal,
                                                                                      @RequestParam(required = false) String title, @RequestParam(required = false) String status,
                                                                                      @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
                                                                                      @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
                                                                                      @RequestParam(defaultValue = "0") int page, @RequestParam(defaultValue = "10") int size,
                                                                                      @RequestParam(defaultValue = "createdAt") String sortBy,
                                                                                      @RequestParam(defaultValue = "desc") String sortDir) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        Integer departmentId = user.getAccount().getDepartment().getId();
        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        QuestionFilterStatus filterStatus = null;
        if (status != null && !status.isEmpty()) {
            filterStatus = QuestionFilterStatus.fromKey(status);
        }

        Page<MyQuestionDTO> questions = questionService.getDepartmentConsultantsQuestionsFilters(departmentId, title,
                filterStatus != null ? filterStatus.getKey() : null, startDate, endDate, pageable);

        if (questions == null || questions.isEmpty()) {
            throw new ErrorException("Không tìm thấy câu hỏi.");
        }

        return DataResponse.<Page<MyQuestionDTO>>builder().status("success")
                .message("Lấy danh sách câu hỏi thành công.").data(questions).build();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/advisor/all-deletion-log/list")
    public ResponseEntity<DataResponse<Page<DeletionLogEntity>>> getDeletionLogsByDepartment(
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "deletedAt") String sortBy,
            @RequestParam(defaultValue = "desc") String sortDir,
            Principal principal) {

        String email = principal.getName();
        UserInformationEntity manager = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Không tìm thấy trưởng ban tư vấn"));

        Integer departmentId = manager.getAccount().getDepartment().getId();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        Page<DeletionLogEntity> logs = questionService.getDeletionLogsByDepartment(departmentId, pageable);

        return ResponseEntity.ok(DataResponse.<Page<DeletionLogEntity>>builder()
                .status("success")
                .message("Lấy lý do xóa thành công.")
                .data(logs)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/advisor/question/detail")
    public ResponseEntity<DataResponse<MyQuestionDTO>> getQuestionByIdAndDepartment(@RequestParam("id") Integer questionId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = userOpt.get();
        Integer departmentId = manager.getAccount().getDepartment().getId();

        MyQuestionDTO questionDTO = questionService.getQuestionByIdAndDepartment(questionId, departmentId);
        if (questionDTO == null) {
            throw new ErrorException("Không tìm thấy câu hỏi");
        }

        return ResponseEntity.ok(DataResponse.<MyQuestionDTO>builder()
                .status("success")
                .message("Lấy chi tiết câu hỏi thành công.")
                .data(questionDTO)
                .build());
    }

}
