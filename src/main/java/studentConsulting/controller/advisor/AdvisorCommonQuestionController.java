package studentConsulting.controller.advisor;

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
import studentConsulting.model.entity.department_field.DepartmentEntity;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.question_answer.CommonQuestionDTO;
import studentConsulting.model.payload.request.question_answer.UpdateCommonQuestionRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.advisor.IAdvisorCommonQuestionService;

import java.security.Principal;
import java.time.LocalDate;
import java.util.Optional;

@RestController
@RequestMapping("${base.url}")
public class AdvisorCommonQuestionController {

    @Autowired
    private IAdvisorCommonQuestionService commonQuestionService;

    @Autowired
    private UserRepository userRepository;

    @GetMapping("/list-common-question")
    public ResponseEntity<DataResponse<Page<CommonQuestionDTO>>> getCommonQuestions(
            @RequestParam(required = false) Integer departmentId,
            @RequestParam(required = false) String title,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "title") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<CommonQuestionDTO> commonQuestions = commonQuestionService.getCommonQuestionsWithFilters(departmentId, title, startDate, endDate, pageable);

        if (commonQuestions.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<CommonQuestionDTO>>builder()
                            .status("error")
                            .message("Câu hỏi chung không tìm thấy")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<CommonQuestionDTO>>builder()
                        .status("success")
                        .message("Lấy câu hỏi chung thành công")
                        .data(commonQuestions)
                        .build()
        );
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @PostMapping("/advisor/common-question/convert-to-common")
    public ResponseEntity<DataResponse<CommonQuestionDTO>> convertToCommonQuestion(@RequestParam Integer questionId, Principal principal) {
        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        CommonQuestionDTO commonQuestion = commonQuestionService.convertToCommonQuestion(questionId, principal);

        if (commonQuestion == null) {
            throw new ErrorException("Không tìm thấy câu hỏi với ID: " + questionId);
        }

        return ResponseEntity.ok(DataResponse.<CommonQuestionDTO>builder()
                .status("success")
                .message("Chuyển đổi câu hỏi thành công.")
                .data(commonQuestion)
                .build());
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @PutMapping(value = "/advisor/common-question/update", consumes = {"multipart/form-data"})
    public DataResponse<CommonQuestionDTO> updateCommonQuestion(
            @RequestParam("commonQuestionId") Integer commonQuestionId,
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestPart(value = "fileName", required = false) MultipartFile fileName,
            @RequestParam("answerTitle") String answerTitle,
            @RequestParam("answerContent") String answerContent,
            Principal principal) {

        String email = principal.getName();
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = userOpt.get();
        Integer departmentId = manager.getAccount().getDepartment().getId();

        UpdateCommonQuestionRequest commonQuestionRequest = UpdateCommonQuestionRequest.builder()
                .title(title)
                .content(content)
                .fileName(fileName)
                .answerTitle(answerTitle)
                .answerContent(answerContent)
                .build();

        CommonQuestionDTO updatedCommonQuestionDTO = commonQuestionService.updateCommonQuestion(commonQuestionId, departmentId, commonQuestionRequest);

        return DataResponse.<CommonQuestionDTO>builder()
                .status("success")
                .message("Cập nhật câu hỏi tổng hợp thành công.")
                .data(updatedCommonQuestionDTO)
                .build();
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @GetMapping("/advisor/list-common-question")
    public ResponseEntity<DataResponse<Page<CommonQuestionDTO>>> getCommonQuestionsByAdvisor(
            @RequestParam(required = false) String title,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "title") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            Principal principal) {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));
        String email = principal.getName();

        Optional<UserInformationEntity> managerOpt = userRepository.findUserInfoByEmail(email);
        if (!managerOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = managerOpt.get();
        DepartmentEntity department = manager.getAccount().getDepartment();

        if (department == null) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<CommonQuestionDTO>>builder()
                            .status("error")
                            .message("Người dùng không thuộc phòng ban nào")
                            .build()
            );
        }
        Page<CommonQuestionDTO> commonQuestions = commonQuestionService.getCommonQuestionsWithAdvisorFilters(department.getId(), title, startDate, endDate, pageable);

        if (commonQuestions.isEmpty()) {
            return ResponseEntity.status(404).body(
                    DataResponse.<Page<CommonQuestionDTO>>builder()
                            .status("error")
                            .message("Không tìm thấy câu hỏi chung")
                            .build()
            );
        }

        return ResponseEntity.ok(
                DataResponse.<Page<CommonQuestionDTO>>builder()
                        .status("success")
                        .message("Lấy câu hỏi chung thành công")
                        .data(commonQuestions)
                        .build()
        );
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.TRUONGBANTUVAN)
    @DeleteMapping("/advisor/common-question/delete")
    public ResponseEntity<DataResponse<Void>> deleteCommonQuestion(@RequestParam Integer id, Principal principal) {
        String email = principal.getName();
        Optional<UserInformationEntity> managerOpt = userRepository.findUserInfoByEmail(email);
        if (!managerOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }

        UserInformationEntity manager = managerOpt.get();
        Integer departmentId = manager.getAccount().getDepartment().getId();

        commonQuestionService.deleteCommonQuestion(id, departmentId);
        return ResponseEntity.ok(DataResponse.<Void>builder()
                .status("success")
                .message("Xóa câu hỏi tổng hợp thành công.")
                .build());
    }

}