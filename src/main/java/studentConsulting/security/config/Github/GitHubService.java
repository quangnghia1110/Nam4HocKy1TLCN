package studentConsulting.security.config.Github;

import okhttp3.*;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.util.Base64;

@Service
public class GitHubService {

    private final String repository = "quangnghia1110/TLCN";
    private final String branch = "main";
    private final String githubToken = "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";

    private final OkHttpClient client = new OkHttpClient();

    public String uploadFile(MultipartFile file) throws IOException {
        String originalFileName = file.getOriginalFilename();
        String fileName = originalFileName != null ? originalFileName.trim() : "default.txt";  // Loại bỏ khoảng trắng không cần thiết và xử lý nếu tên tệp null

        System.out.println("File name length: " + fileName.length());
        System.out.println("File name: " + fileName);

        String shortenedFileName = fileName.substring(0, Math.min(fileName.length(), 255));
        String filePath = "files/" + shortenedFileName;

        String content = Base64.getEncoder().encodeToString(file.getBytes());

        String existingFileSha = getExistingFileSha(filePath);

        String jsonPayload;
        if (existingFileSha != null) {
            jsonPayload = String.format(
                    "{\"message\":\"Update %s\",\"branch\":\"%s\",\"content\":\"%s\",\"sha\":\"%s\"}",
                    shortenedFileName, branch, content, existingFileSha
            );
        } else {
            jsonPayload = String.format(
                    "{\"message\":\"Add %s\",\"branch\":\"%s\",\"content\":\"%s\"}",
                    shortenedFileName, branch, content
            );
        }

        Request request = new Request.Builder()
                .url("https://api.github.com/repos/" + repository + "/contents/" + filePath)
                .header("Authorization", "token " + githubToken)
                .header("Accept", "application/vnd.github.v3+json")
                .put(RequestBody.create(jsonPayload, MediaType.parse("application/json; charset=utf-8")))
                .build();

        try (Response response = client.newCall(request).execute()) {
            if (!response.isSuccessful()) {
                throw new IOException("Unexpected code " + response);
            }

            // Trả về trực tiếp URL đến file trên GitHub
            return "https://github.com/" + repository + "/blob/" + branch + "/" + filePath;
        }
    }

    private String getExistingFileSha(String filePath) throws IOException {
        Request request = new Request.Builder()
                .url("https://api.github.com/repos/" + repository + "/contents/" + filePath)
                .header("Authorization", "token " + githubToken)
                .header("Accept", "application/vnd.github.v3+json")
                .get()
                .build();

        try (Response response = client.newCall(request).execute()) {
            if (response.isSuccessful()) {
                String responseBody = response.body().string();
                // Chỉ cần lấy thông tin SHA từ phản hồi
                int shaIndex = responseBody.indexOf("\"sha\":\"") + 7;
                int endIndex = responseBody.indexOf("\"", shaIndex);
                return responseBody.substring(shaIndex, endIndex);
            } else if (response.code() == 404) {
                return null;  // File does not exist
            } else {
                throw new IOException("Unexpected code " + response);
            }
        }
    }
}
