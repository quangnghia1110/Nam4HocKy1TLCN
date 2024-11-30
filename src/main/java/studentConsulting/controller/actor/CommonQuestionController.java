package studentConsulting.controller.actor;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.constant.enums.NotificationContent;
import studentConsulting.constant.enums.NotificationType;
import studentConsulting.model.entity.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.actor.CommonQuestionDTO;
import studentConsulting.model.payload.request.CommonQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.admin.UserRepository;
import studentConsulting.service.interfaces.actor.ICommonQuestionService;
import studentConsulting.service.interfaces.common.IExcelService;
import studentConsulting.service.interfaces.common.INotificationService;
import studentConsulting.service.interfaces.common.IPdfService;

import java.security.Principal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class CommonQuestionController {

    @Autowired
    private ICommonQuestionService commonQuestionService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private IExcelService excelService;

    @Autowired
    private IPdfService pdfService;

    @Autowired
    private INotificationService notificationService;

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor-admin/list-common-question")
    public ResponseEntity<DataResponse<Page<CommonQuestionDTO>>> getCommonQuestionsByAdvisor(
            @RequestParam(required = false) String title,
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
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        Integer departmentId = isAdmin ? null : user.getAccount().getDepartment().getId();

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<CommonQuestionDTO> commonQuestions = commonQuestionService.getCommonQuestionByRole(departmentId, title, startDate, endDate, pageable);

        return ResponseEntity.ok(
                DataResponse.<Page<CommonQuestionDTO>>builder()
                        .status("success")
                        .message("Lấy câu hỏi chung thành công")
                        .data(commonQuestions)
                        .build()
        );
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/common-question/convert-to-common")
    public ResponseEntity<DataResponse<CommonQuestionDTO>> convertToCommonQuestion(@RequestParam Integer questionId, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();

        CommonQuestionDTO commonQuestion = commonQuestionService.convertToCommonQuestion(questionId, principal);

        if (commonQuestion == null) {
            throw new ErrorException("Không tìm thấy câu hỏi với ID: " + questionId);
        }

        if (!user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN)) {
            List<UserInformationEntity> admins = userRepository.findAllByRole(SecurityConstants.Role.ADMIN);
            for (UserInformationEntity admin : admins) {
                notificationService.sendUserNotification(
                        user.getId(),
                        admin.getId(),
                        NotificationContent.NEW_COMMON_QUESTION.formatMessage(user.getLastName() + " " + user.getFirstName()),
                        NotificationType.ADMIN
                );
            }
        }

        return ResponseEntity.ok(DataResponse.<CommonQuestionDTO>builder()
                .status("success")
                .message("Chuyển đổi câu hỏi thành công.")
                .data(commonQuestion)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PutMapping(value = "/advisor-admin/common-question/update", consumes = {"multipart/form-data"})
    public DataResponse<CommonQuestionDTO> updateCommonQuestion(
            @RequestParam("commonQuestionId") Integer commonQuestionId,
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestPart(value = "file", required = false) MultipartFile fileName,
            @RequestParam("answerTitle") String answerTitle,
            @RequestParam("answerContent") String answerContent,
            @RequestParam("answerCreatedAt") LocalDate answerCreatedAt,
            @RequestParam("answerUserLastname") String answerUserLastname,
            @RequestParam("answerUserFirstname") String answerUserFirstname,
            @RequestParam("askerLastname") String askerLastname,
            @RequestParam("askerFirstname") String askerFirstname,
            @RequestParam("answerUserEmail") String answerUserEmail,
            Principal principal) {

        CommonQuestionRequest commonQuestionRequest = CommonQuestionRequest.builder()
                .title(title)
                .content(content)
                .fileName(fileName)
                .answerTitle(answerTitle)
                .answerContent(answerContent)
                .answerCreatedAt(answerCreatedAt)
                .answerUserLastname(answerUserLastname)
                .answerUserFirstname(answerUserFirstname)
                .askerLastname(askerLastname)
                .askerFirstname(askerFirstname)
                .answerEmail(answerUserEmail)
                .build();

        CommonQuestionDTO updatedCommonQuestionDTO = commonQuestionService.updateCommonQuestion(commonQuestionId, commonQuestionRequest, principal);

        return DataResponse.<CommonQuestionDTO>builder()
                .status("success")
                .message("Cập nhật câu hỏi tổng hợp thành công.")
                .data(updatedCommonQuestionDTO)
                .build();
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @DeleteMapping("/advisor-admin/common-question/delete")
    public ResponseEntity<DataResponse<Void>> deleteCommonQuestion(@RequestParam Integer id, Principal principal) {
        String email = principal.getName();
        UserInformationEntity user = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng"));

        commonQuestionService.deleteCommonQuestion(id, user);

        return ResponseEntity.ok(DataResponse.<Void>builder()
                .status("success")
                .message("Xóa câu hỏi chung thành công.")
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @GetMapping("/advisor-admin/common-question/detail")
    public ResponseEntity<DataResponse<CommonQuestionDTO>> getCommonQuestionById(@RequestParam("id") Integer questionId, Principal principal) {
        String email = principal.getName();
        UserInformationEntity user = userRepository.findUserInfoByEmail(email)
                .orElseThrow(() -> new ErrorException("Không tìm thấy người dùng"));

        CommonQuestionDTO questionDTO = commonQuestionService.getCommonQuestionById(questionId, user);

        return ResponseEntity.ok(DataResponse.<CommonQuestionDTO>builder()
                .status("success")
                .data(questionDTO)
                .build());
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN + " or " + SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/advisor-admin/common-question/create")
    public ResponseEntity<DataResponse<CommonQuestionDTO>> createCommonQuestion(@RequestPart(value = "file", required = false) MultipartFile file,
                                                                                @RequestBody CommonQuestionRequest request, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);

        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity user = userOpt.get();
        boolean isAdmin = user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);

        if (!isAdmin) {
            request.setDepartmentId(user.getAccount().getDepartment().getId());
        }

        CommonQuestionDTO commonQuestionDTO = commonQuestionService.createCommonQuestion(request, file, principal);

        return ResponseEntity.ok(DataResponse.<CommonQuestionDTO>builder()
                .status("success")
                .message("Tạo câu hỏi chung mới thành công.")
                .data(commonQuestionDTO)
                .build());
    }

}