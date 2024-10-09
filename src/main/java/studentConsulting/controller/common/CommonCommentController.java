package studentConsulting.controller.common;

import com.lowagie.text.DocumentException;
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
import studentConsulting.constant.FilePaths;
import studentConsulting.constant.SecurityConstants;
import studentConsulting.model.entity.user.UserInformationEntity;
import studentConsulting.model.exception.Exceptions.ErrorException;
import studentConsulting.model.payload.dto.content.CommentDTO;
import studentConsulting.model.payload.response.DataResponse;
import studentConsulting.repository.user.UserRepository;
import studentConsulting.service.interfaces.common.ICommonCommentService;
import studentConsulting.service.interfaces.common.ICommonExcelService;
import studentConsulting.service.interfaces.common.ICommonPdfService;

import javax.servlet.http.HttpServletResponse;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.security.Principal;
import java.time.LocalDate;
import java.util.Hashtable;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.stream.Collectors;

@RestController
@RequestMapping("${base.url}")
public class CommonCommentController {

    @Autowired
    private UserRepository userRepository;

    @Autowired
    private ICommonCommentService commentService;

    @Autowired
    private ICommonExcelService excelService;

    @Autowired
    private ICommonPdfService pdfService;

    @PostMapping("/comment/create")
    public ResponseEntity<DataResponse<CommentDTO>> createComment(@RequestParam Integer postId,
                                                                  @RequestParam String text, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        CommentDTO createdComment = commentService.createComment(postId, text, email);

        return ResponseEntity.ok(DataResponse.<CommentDTO>builder().status("success")
                .message("Bình luận đã được tạo thành công").data(createdComment).build());
    }

    @PostMapping("/comment/reply")
    public ResponseEntity<DataResponse<CommentDTO>> replyComment(@RequestParam Integer commentFatherId,
                                                                 @RequestParam String text, Principal principal) {

        String email = principal.getName();
        System.out.println("Email: " + email);
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (!userOpt.isPresent()) {
            throw new ErrorException("Không tìm thấy người dùng");
        }
        CommentDTO replyComment = commentService.replyComment(commentFatherId, text, email);

        return ResponseEntity.ok(DataResponse.<CommentDTO>builder().status("success")
                .message("Bình luận đã được trả lời thành công").data(replyComment).build());
    }

    private boolean isAdminFromDB(String email) {
        Optional<UserInformationEntity> userOpt = userRepository.findUserInfoByEmail(email);
        if (userOpt.isPresent()) {
            UserInformationEntity user = userOpt.get();
            return user.getAccount().getRole().getName().equals(SecurityConstants.Role.ADMIN);
        }
        return false;
    }

    @PutMapping("/comment/update")
    public ResponseEntity<DataResponse<Hashtable<String, Object>>> updateComment(@RequestParam Integer commentId,
                                                                                 @RequestParam String text,
                                                                                 Principal principal) {

        String email = principal.getName();
        boolean isAdmin = isAdminFromDB(email);

        Hashtable<String, Object> updatedComment;
        if (isAdmin) {
            updatedComment = commentService.adminUpdateComment(commentId, text);
        } else {
            updatedComment = commentService.updateComment(commentId, text, email);
        }

        return ResponseEntity.ok(DataResponse.<Hashtable<String, Object>>builder().status("success")
                .message("Bình luận đã được cập nhật thành công").data(updatedComment).build());
    }

    @DeleteMapping("/comment/delete")
    public ResponseEntity<DataResponse<Void>> deleteComment(@RequestParam Integer commentId, Principal principal) {

        String email = principal.getName();
        boolean isAdmin = isAdminFromDB(email);

        if (isAdmin) {
            commentService.adminDeleteComment(commentId);
        } else {
            commentService.deleteComment(commentId, email);
        }

        return ResponseEntity
                .ok(DataResponse.<Void>builder().status("success").message("Bình luận đã được xóa").build());
    }

    @GetMapping("/comment/get-comment-by-post")
    public ResponseEntity<DataResponse<List<Hashtable<String, Object>>>> getCommentsByPost(
            @RequestParam Integer postId) {
        DataResponse<List<Hashtable<String, Object>>> response = commentService.getAllComments(postId);

        if (response.getData().isEmpty()) {
            throw new ErrorException("Không có bình luận nào cho bài viết này.");
        }

        return new ResponseEntity<>(response, HttpStatus.OK);
    }

    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-comment-csv")
    public void exportCommentsToCsv(
            @RequestParam(required = false) Integer postId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createDate") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<CommentDTO> commentPage = commentService.getCommentsByPostWithPagingAndFilters(Optional.ofNullable(postId), Optional.ofNullable(startDate), Optional.ofNullable(endDate), pageable);
        List<CommentDTO> comments = commentPage.getContent();

        if (comments.isEmpty()) {
            throw new ErrorException("Không có bình luận nào để xuất");
        }

        List<String> headers = List.of("Content", "User ID", "Created At", "Post ID");

        List<List<String>> data = comments.stream()
                .map(comment -> List.of(
                        comment.getText() != null ? comment.getText() : "N/A",
                        comment.getUser().getId() != null ? comment.getUser().getId().toString() : "N/A",
                        comment.getCreate_date() != null ? comment.getCreate_date().toString() : "N/A",
                        comment.getPostId() != null ? comment.getPostId().toString() : "N/A"
                ))
                .collect(Collectors.toList());

        String fileName = "Comments_" + excelService.currentDate() + ".csv";

        excelService.generateExcelFile("Comments", headers, data, fileName, response);
    }


    @PreAuthorize(SecurityConstants.PreAuthorize.ADMIN)
    @PostMapping("/admin/export-comment-pdf")
    public void exportCommentsToPdf(
            @RequestParam(required = false) Integer postId,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate startDate,
            @RequestParam(required = false) @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate endDate,
            @RequestParam(defaultValue = "0") int page,
            @RequestParam(defaultValue = "10") int size,
            @RequestParam(defaultValue = "createDate") String sortBy,
            @RequestParam(defaultValue = "asc") String sortDir,
            HttpServletResponse response) throws DocumentException, IOException {

        Pageable pageable = PageRequest.of(page, size, Sort.by(Sort.Direction.fromString(sortDir), sortBy));

        Page<CommentDTO> commentPage = commentService.getCommentsByPostWithPagingAndFilters(Optional.ofNullable(postId), Optional.ofNullable(startDate), Optional.ofNullable(endDate), pageable);
        List<CommentDTO> comments = commentPage.getContent();

        if (comments.isEmpty()) {
            throw new IOException("Không có bình luận nào để xuất");
        }

        String templatePath = "/templates/comment_template.html";
        String dataRows = buildCommentDataRows(comments);

        Map<String, String> placeholders = Map.of(
                "{{date}}", pdfService.currentDate(),
                "{{comments}}", dataRows,
                "{{logo_url}}", FilePaths.LOGO_URL
        );

        String fileName = "Comments_" + pdfService.currentDate() + ".pdf";
        String outputFilePath = FilePaths.PDF_OUTPUT_DIRECTORY + fileName;

        try (OutputStream fileOutputStream = new FileOutputStream(outputFilePath)) {
            pdfService.generatePdfFromTemplate(templatePath, placeholders, fileOutputStream);
        } catch (IOException | DocumentException e) {
            throw new IOException("Lỗi khi tạo hoặc lưu file PDF", e);
        }

        try (OutputStream responseStream = response.getOutputStream()) {
            pdfService.generatePdfFromTemplate(templatePath, placeholders, responseStream);
            response.flushBuffer();
        } catch (IOException | DocumentException e) {
            throw new IOException("Lỗi khi gửi file PDF qua HTTP response", e);
        }
    }


    private String buildCommentDataRows(List<CommentDTO> comments) {
        StringBuilder dataRows = new StringBuilder();

        for (CommentDTO comment : comments) {
            dataRows.append("<tr>")
                    .append("<td>").append(comment.getText()).append("</td>")
                    .append("<td>").append(comment.getUser().getLastName()).append(" ")
                    .append(comment.getUser().getFirstName()).append("</td>")
                    .append("<td>").append(comment.getCreate_date()).append("</td>")
                    .append("<td>").append(comment.getPostId()).append("</td>")
                    .append("</tr>");
        }

        return dataRows.toString();
    }

}
