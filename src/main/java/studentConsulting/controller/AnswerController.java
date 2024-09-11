package studentConsulting.controller;

import java.security.Principal;
import java.util.Optional;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import studentConsulting.model.entity.authentication.UserInformationEntity;
import studentConsulting.model.entity.questionAnswer.AnswerEntity;
import studentConsulting.model.entity.roleBaseAction.RoleConsultantEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.AnswerDTO;
import studentConsulting.model.payload.request.answer.CreateAnswerRequest;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.AnswerRepository;
import studentConsulting.repository.QuestionRepository;
import studentConsulting.repository.UserRepository;
import studentConsulting.service.IAnswerService;

@RestController
@RequestMapping("${base.url}")
public class AnswerController {

    @Autowired
    private IAnswerService answerService;

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private AnswerRepository answerRepository;

    @PreAuthorize("hasRole('TUVANVIEN')")
    @PostMapping(value = "/consultant/answer/create", consumes = { "multipart/form-data" })
    public ResponseEntity<DataResponse<AnswerDTO>> createAnswer(
            @RequestParam("questionId") Integer questionId,
            @RequestParam("title") String title,
            @RequestParam("content") String content,
            @RequestPart("file") MultipartFile file,
            @RequestParam("statusApproval") Boolean statusApproval,
            Principal principal) {

        String username = principal.getName();

        Optional<UserInformationEntity> userOpt = userRepository.findByAccountUsername(username);
        if (userOpt.isEmpty()) {
            throw new ErrorException("Người dùng không tồn tại.");
        }

        UserInformationEntity user = userOpt.get();
        RoleConsultantEntity roleConsultant = user.getAccount().getRoleConsultant();

        CreateAnswerRequest answerRequest = CreateAnswerRequest.builder()
                .questionId(questionId)
                .title(title)
                .content(content)
                .file(file)
                .statusApproval(statusApproval)
                .roleConsultantId(roleConsultant.getId())
                .consultantId(user.getId())
                .build();

        AnswerDTO answerDTO = answerService.createAnswer(answerRequest);

        return ResponseEntity.ok(DataResponse.<AnswerDTO>builder()
                .status("success")
                .message("Trả lời thành công.")
                .data(answerDTO)
                .build());
    }

    @PreAuthorize("hasRole('TRUONGBANTUVAN')")
    @PostMapping("/advisor/answer/review")
    public ResponseEntity<DataResponse<AnswerDTO>> reviewAnswer(
            @RequestBody CreateAnswerRequest reviewRequest, Principal principal) {

        String username = principal.getName();

        Optional<UserInformationEntity> userOpt = userRepository.findByAccountUsername(username);
        if (userOpt.isEmpty()) {
            throw new ErrorException("Người dùng không tồn tại.");
        }

        UserInformationEntity user = userOpt.get();

        if (!isConsultant(user)) {
            throw new ErrorException("Bạn không có quyền kiểm duyệt câu trả lời này.");
        }

        Optional<AnswerEntity> answerOpt = answerRepository.findFirstAnswerByQuestionId(reviewRequest.getQuestionId());
        if (answerOpt.isEmpty()) {
            throw new ErrorException("Không tìm thấy câu trả lời cho câu hỏi này.");
        }

        AnswerEntity answer = answerOpt.get();

        if (answer.getStatusAnswer() != null && answer.getStatusAnswer()) {
            throw new ErrorException("Câu trả lời này đã được duyệt và không thể kiểm duyệt lại.");
        }

        UserInformationEntity consultant = answer.getUser();

        // Kiểm tra xem trưởng ban có cùng department với tư vấn viên hay không
        if (!consultant.getAccount().getDepartment().getId().equals(user.getAccount().getDepartment().getId())) {
            throw new ErrorException("Bạn không có quyền kiểm duyệt câu trả lời từ bộ phận khác.");
        }

        AnswerDTO reviewedAnswer = answerService.reviewAnswer(reviewRequest);

        return ResponseEntity.ok(DataResponse.<AnswerDTO>builder()
            .status("success")
            .message("Kiểm duyệt thành công")
            .data(reviewedAnswer)
            .build());
    }

    private boolean isConsultant(UserInformationEntity user) {
        return "TRUONGBANTUVAN".equals(user.getAccount().getRole().getName());
    }
}